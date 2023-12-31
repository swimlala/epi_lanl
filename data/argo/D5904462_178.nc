CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-21T12:59:56Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         (Matthew Alkire, University of Washington      @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7,   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  74   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7t   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     88   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8X   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8x   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           8|   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �|   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �|   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �t   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �0   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �4   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �D   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �H   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �L   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20181121125956  20190405100800  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  5287                            2C  D   APEX                            6529                            072314                          846 @�����1   @��̛b@0"��`B�c�E���1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @�  @���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C�C�C�C  C  C   C"  C#�fC&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"fD"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE�fDF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� DcfDc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm�fDn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt� Dy��D�3D�S3D�� D��fD�fD�P D�|�D���D�3D�P D��3D���D���D�C3Dړ3D�� D�3D�<�D�|�D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�  @���A  A(  AH  Ah  A�  A�  A�  A�  A�  A�  A�  A�  B  B
  B  B  B"  B*  B2  B:  BB  BJ  BR  BZ  Bb  Bj  Br  Bz  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C � C� C� C� C� C
� C� C� C� C� C� C��C��C��C� C� C � C"� C$ffC&� C(� C*� C,� C.� C0� C2� C4� C6� C8� C:� C<� C>� C@� CB� CD� CF� CH� CJ� CL� CN� CP� CR� CT� CV� CX� CZ� C\� C^� C`� Cb� Cd� Cf� Ch� Cj� Cl� Cn� Cp� Cr� Ct� Cv� Cx� Cz� C|� C~� C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�L�C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"&fD"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE�fDF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc&fDc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm�fDn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  DyٚD�3D�c3D�� D��fD�fD�` D���D���D�#3D�` D��3D���D��D�S3Dڣ3D�� D�#3D�L�D��D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AɸRAɸRAɸRAɺ^Aɺ^AɼjAɾwAɾwAɸRAɓuA�Aȥ�A�M�A��A�oA��A��A��A�A��yA��/A��mA���A���A�A��A���A���A���A�ȴAǸRAǲ-AǬAǩ�Aǰ!AǺ^A���A�ĜAǺ^AǋDA�r�A�VA�$�A�JAƸRA�x�A�{A�^5Aº^A�n�A�l�A�ffA�^5A���A���A��A�M�A�~�A�jA�?}A�XA��A�VA��;A�C�A�
=A��uA��A���A�bA��A�
=A�v�A�ƨA��`A���A��wA���A��TA��-A��`A�5?A���A��A��A�K�A��A���A�A�A�ZA�A��mA�~�A��A���A�ĜA|ffAx�RAt��Ap��Al1Ak�Ah��Aa�A]��AZ��AY"�AV9XAS`BAOAK�FAI;dAFbNAB�/AAhsA?G�A<��A8��A97LA8z�A6bNA5�FA4A3�A1��A0�`A0�A/�^A/A.bNA.r�A.A�A-�FA-�A,ZA,��A,9XA*��A'ƨA&�A&��A&ĜA&��A'�A'
=A't�A$~�A"^5A ��A!�wA"-A"��A#�A#�#A#�A#��A"�9A!�A!��A!�hA!�FA!��A!G�A �jA ��A z�Al�A�A��A�A
=A�A�7A?}A��AVA��A
=A�HA�AE�AbA��A�hA�A��Ar�A9XAA%A~�A�A|�A\)AO�A�AQ�A�;A�-AhsA�A��A%Av�A�#A/AI�A��A`BAQ�Ax�A�7A�A�hA
n�A�A�TA��A\)A{A?}@�\)@��A A�A�FA�RA%AĜA�PA��A��A�A �@���@�X@��h@��@��@�n�@���@�1@�dZ@��#@�V@�@�b@�P@��@�ȴ@��@�@�/@�@���@�E�@�(�@�F@�  @�ƨ@�@ꗍ@���@�@��@�K�@�ȴ@�V@旍@���@��@�o@�M�@�@��H@�M�@��T@�%@��u@��;@�;d@�o@�v�@�$�@�@�%@�9X@ۮ@�"�@�ȴ@��T@�/@�b@���@�o@�~�@���@Ձ@�?}@�V@ԛ�@�z�@�(�@ӶF@�ff@��@�bN@��@�33@�v�@��@͡�@���@͉7@�j@�(�@��
@˶F@˅@ʇ+@��#@ɑh@�7L@�&�@ȣ�@�  @��@�~�@�V@�J@���@�?}@��
@�"�@��H@�V@��T@�V@��@�(�@��@�t�@�|�@�dZ@��\@�$�@��@�/@�9X@��
@��y@��+@��@���@�X@�(�@�V@�J@��#@���@��-@��h@��j@���@���@���@�|�@�l�@�S�@�K�@�o@��H@���@�
=@���@�$�@��@��7@�p�@�`B@�7L@���@��@�r�@�  @��;@��w@�l�@�o@�E�@��@�5?@�n�@�J@���@�@�X@��9@� �@���@�l�@��@�~�@��@��^@��7@�X@�?}@��9@� �@��
@��w@���@�33@���@�~�@�n�@�M�@�E�@��@��^@��h@�%@���@�Z@�b@��P@�+@�
=@���@�M�@�{@��@�&�@��@���@�Q�@��@�ƨ@�dZ@��y@��+@���@�p�@�&�@���@��@���@���@�r�@��@���@�K�@�+@�"�@��@���@��+@�5?@��@�@��@���@��9@�Z@��@��m@��F@��@�t�@�l�@�C�@�@���@���@���@��+@�ff@�ff@�-@�p�@�O�@��@�z�@�Q�@�9X@�(�@��@��;@���@���@���@��P@�33@{o@p��@f��@]@R=q@JM�@?;d@8�@1��@+�
@'+@!��@�+@G�@z�@r�@(�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  AɸRAɸRAɸRAɺ^Aɺ^AɼjAɾwAɾwAɸRAɓuA�Aȥ�A�M�A��A�oA��A��A��A�A��yA��/A��mA���A���A�A��A���A���A���A�ȴAǸRAǲ-AǬAǩ�Aǰ!AǺ^A���A�ĜAǺ^AǋDA�r�A�VA�$�A�JAƸRA�x�A�{A�^5Aº^A�n�A�l�A�ffA�^5A���A���A��A�M�A�~�A�jA�?}A�XA��A�VA��;A�C�A�
=A��uA��A���A�bA��A�
=A�v�A�ƨA��`A���A��wA���A��TA��-A��`A�5?A���A��A��A�K�A��A���A�A�A�ZA�A��mA�~�A��A���A�ĜA|ffAx�RAt��Ap��Al1Ak�Ah��Aa�A]��AZ��AY"�AV9XAS`BAOAK�FAI;dAFbNAB�/AAhsA?G�A<��A8��A97LA8z�A6bNA5�FA4A3�A1��A0�`A0�A/�^A/A.bNA.r�A.A�A-�FA-�A,ZA,��A,9XA*��A'ƨA&�A&��A&ĜA&��A'�A'
=A't�A$~�A"^5A ��A!�wA"-A"��A#�A#�#A#�A#��A"�9A!�A!��A!�hA!�FA!��A!G�A �jA ��A z�Al�A�A��A�A
=A�A�7A?}A��AVA��A
=A�HA�AE�AbA��A�hA�A��Ar�A9XAA%A~�A�A|�A\)AO�A�AQ�A�;A�-AhsA�A��A%Av�A�#A/AI�A��A`BAQ�Ax�A�7A�A�hA
n�A�A�TA��A\)A{A?}@�\)@��A A�A�FA�RA%AĜA�PA��A��A�A �@���@�X@��h@��@��@�n�@���@�1@�dZ@��#@�V@�@�b@�P@��@�ȴ@��@�@�/@�@���@�E�@�(�@�F@�  @�ƨ@�@ꗍ@���@�@��@�K�@�ȴ@�V@旍@���@��@�o@�M�@�@��H@�M�@��T@�%@��u@��;@�;d@�o@�v�@�$�@�@�%@�9X@ۮ@�"�@�ȴ@��T@�/@�b@���@�o@�~�@���@Ձ@�?}@�V@ԛ�@�z�@�(�@ӶF@�ff@��@�bN@��@�33@�v�@��@͡�@���@͉7@�j@�(�@��
@˶F@˅@ʇ+@��#@ɑh@�7L@�&�@ȣ�@�  @��@�~�@�V@�J@���@�?}@��
@�"�@��H@�V@��T@�V@��@�(�@��@�t�@�|�@�dZ@��\@�$�@��@�/@�9X@��
@��y@��+@��@���@�X@�(�@�V@�J@��#@���@��-@��h@��j@���@���@���@�|�@�l�@�S�@�K�@�o@��H@���@�
=@���@�$�@��@��7@�p�@�`B@�7L@���@��@�r�@�  @��;@��w@�l�@�o@�E�@��@�5?@�n�@�J@���@�@�X@��9@� �@���@�l�@��@�~�@��@��^@��7@�X@�?}@��9@� �@��
@��w@���@�33@���@�~�@�n�@�M�@�E�@��@��^@��h@�%@���@�Z@�b@��P@�+@�
=@���@�M�@�{@��@�&�@��@���@�Q�@��@�ƨ@�dZ@��y@��+@���@�p�@�&�@���@��@���@���@�r�@��@���@�K�@�+@�"�@��@���@��+@�5?@��@�@��@���@��9@�Z@��@��m@��F@��@�t�@�l�@�C�@�@���@���@���@��+@�ff@�ff@�-@�p�@�O�@��@�z�@�Q�@�9X@�(�@��@��;G�O�@���@���@��P@�33@{o@p��@f��@]@R=q@JM�@?;d@8�@1��@+�
@'+@!��@�+@G�@z�@r�@(�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B	PB	dZB	ffB	ZB	T�B	T�B	XB	[#B	\)B	[#B	[#B	]/B	bNB	iyB	n�B	y�B	�B	�uB	��B	��B	�-B	�dB	�}B	ŢB	��B	�TB	�B	��B
!�B
T�B
��B
ŢB
�yBuB#�B>wBP�BN�BZB�B�PB��B��BBBB�B2-B9XBI�BR�BW
BXB\)B^5B`BB_;B^5B^5B[#BS�BYBQ�B9XB �BB��B�B�/BǮB��Br�BI�B33B�B
��B
�BB
��B
�^B
�+B
e`B
O�B
B�B
;dB
33B
 �B
	7B	�mB	��B	�B	�{B	w�B	l�B	ZB	1'B	�B	PB	+B��B�B�B�mB�ZB�yB�B��B	oB	%B��B	 �B	0!B	'�B	-B	#�B	�B	�B	+B	:^B	J�B	VB	gmB	w�B	|�B	�=B	��B	��B	�!B	�LB	�B	�hB	�B	�bB	�uB	��B	�B	�!B	B	�9B	�B	�B	��B	�BB	�B
+B
hB
oB
hB
JB

=B

=B
PB
uB
�B
�B
�B
�B
"�B
!�B
"�B
$�B
#�B
�B
�B
�B
�B
�B
�B
�B
hB
oB
uB
uB
uB
uB
�B
�B
�B
�B
{B
{B
�B
�B
{B
�B
�B
�B
{B
{B
{B
oB
bB
DB
VB
uB
\B
DB
%B
B
  B	��B	��B
VB
uB
\B
B	��B	�mB	�5B	�)B	�B	ȴB	�B	�VB	�=B	��B	�qB	��B	��B	�yB	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	�B	�B	�!B	�3B	�FB	�LB	�XB	�XB	�?B	�B	�'B	�^B	�XB	�wB	�qB	�LB	�XB	�qB	�dB	�XB	�qB	��B	ÖB	ȴB	��B	��B	ǮB	ĜB	ÖB	ĜB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�
B	�
B	�B	�B	�B	�B	�B	�B	�B	�
B	�
B	�B	�
B	�#B	�5B	�TB	�TB	�NB	�TB	�TB	�TB	�NB	�NB	�NB	�TB	�TB	�fB	�sB	�sB	�fB	�mB	�yB	�B	�yB	�B	�B	�B	�B	�B	�B	�yB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
1B
1B
	7B
	7B

=B
DB
JB
JB
JB
DB
DB
DB
DB
DB
DB
DB

=B

=B
DB
DB
DB
JB
JB
PB
JB
PB
PB
VB
VB
\B
\B
bB
bB
bB
bB
bB
hB
hB
oB
hB
oB
oB
oB
uB
uB
oB
oB
hB
bB
\B
VB
VB
VB
\B
bB
bB
bB
bB
hB
oB
uB
{B
{B
{B
{B
uB
uB
oB
uB
uB
{B
{B
�B
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
$�B
,B
2-B
6FB
=qB
A�B
I�B
N�B
S�B
YB
]/B
aHB
ffB
iyB
m�B
p�B
v�B
z�B
� B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B��B��B��B��B��B��B��B��B��B	(B	d3B	f<B	Y�B	T�B	T�B	W�B	Z�B	\B	Z�B	Z�B	]B	b#B	iOB	nmB	y�B	��B	�KB	��B	��B	�B	�;B	�TB	�zB	��B	�*B	�B	��B
!�B
T�B
��B
�yB
�QBJB#�B>OBP�BN�BY�B��B�&B��B��B �B�B�BjB2B9-BI�BR�BV�BW�B[�B^B`B_B^B^BZ�BS�BX�BQ�B9,B �B�B��B�UB��BǂB��Br�BI�B3BwB
��B
�B
͢B
�+B
��B
e/B
O�B
B^B
;5B
3B
 �B
	B	�:B	ʐB	��B	�GB	w�B	lWB	Y�B	0�B	jB	B	�B��B�rB�VB�5B�$B�CB�~B��B	9B	�B��B	 �B	/�B	'�B	,�B	#�B	uB	rB	*�B	:#B	J�B	U�B	g4B	w�B	|�B	�B	�OB	�wB	��B	�B	��B	�-B	��B	�(B	�;B	�fB	��B	��B	�UB	��B	��B	��B	ӽB	�B	�IB
�B
/B
6B
-B
B

B

B
B
:B
RB
ZB
HB
MB
"�B
!�B
"�B
$�B
#�B
�B
�B
yB
iB
RB
XB
LB
,B
5B
:B
7B
:B
7B
JB
QB
JB
LB
?B
?B
MB
RB
?B
GB
IB
FB
?B
>B
?B
3B
&B
B
B
;B
 B
	B
�B
�B	��B	��B	��B
B
9B
B
 �B	��B	�0B	��B	��B	��B	�xB	��B	�B	��B	��B	�3B	ʄB	ЧB	�<B	��B	��B	�zB	�GB	�JB	��B	�kB	�{B	�mB	�DB	�IB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	� B	��B	��B	� B	�B	�9B	�3B	�B	�B	�4B	�#B	�B	�3B	�KB	�WB	�uB	ΙB	ˇB	�oB	�`B	�YB	�_B	�oB	�{B	ˉB	͑B	ΛB	ϝB	ϞB	ϟB	ΗB	ϢB	ХB	ѮB	ҵB	ӺB	ҳB	ӺB	��B	ԿB	ԾB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�&B	�5B	�4B	�(B	�.B	�:B	�@B	�9B	�?B	�=B	�KB	�QB	�LB	�EB	�8B	�3B	�2B	�?B	�DB	�SB	�SB	�OB	�QB	�KB	�LB	�KB	�SB	�KB	�]B	�WB	�VB	�PB	�LB	�=B	�EB	�EB	�RB	�WB	�bB	�jB	�cB	�eB	�iB	�jB	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
 �B
�B
�B
�B
�B
 �B
�B
�B
�B
�B
�B
�B
�B
	�B
B

B
	B
	B
B
B
B
B
B
B
B
	�B
	�B
B
B
B
B
B
B
	B
B
B
B
B
B
B
 B
 B
"B
 B
!B
)B
(B
-B
'B
.B
-B
.B
2B
3B
.B
,B
)B
!B
B
B
B
B
B
#B
B
 B
B
(B
.B
2B
:B
:B
;B
:B
5B
4B
-B
6B
4B
<B
<B
?B
@B
@B
<B
@B
@B
@B
AB
BB
NB
LB
LB
PB
YB
dB
gB
_B
^B
_B
_B
WB
`B
`B
bB
^G�O�B
B
$�B
+�B
1�B
6B
=2B
AGB
IyB
N�B
S�B
X�B
\�B
aB
f%B
i7B
mPB
pcB
v�B
z�B
�B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.5 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             201904051008002019040510080020190405100800  AO  ARCAADJP                                                                    20181121125956    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181121125956  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181121125956  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190405100800  IP                  G�O�G�O�G�O�                