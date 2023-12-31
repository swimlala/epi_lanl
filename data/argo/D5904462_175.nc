CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-21T12:59:55Z creation      
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
_FillValue                    ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �    HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �D   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �T   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �X   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �h   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �l   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �p   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20181121125955  20190405100800  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  5287                            2C  D   APEX                            6529                            072314                          846 @���I���1   @�������@/���v��c���l�D1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @333@�  @�  A   A   A@  A`  A�  A�  A�33A�33A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�33C �C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch�Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� DfD� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DX��DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dyl�D�fD�P D�y�D���D�3D�9�D��fD���D�3D�6fD�` Dǣ3D�fD�S3Dک�D��3D�	�D�<�D��D�ɚ1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @S33@�  @�  A  A(  AH  Ah  A�  A�  A�33A�33A�  A�  A�  A�  B  B
  B  B  B"  B*  B2  B:  BB  BJ  BR  BZ  Bb  Bj  Br  Bz  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�33C ��C� C� C� C� C
� C� C� C� C� C� C� C� C� C� C� C � C"� C$� C&� C(� C*� C,� C.� C0� C2� C4� C6� C8� C:� C<� C>� C@� CB� CD� CF� CH� CJ� CL� CN� CP� CR� CT� CV� CX� CZ� C\� C^� C`� Cb� Cd� Cf� Ch��Cj� Cl� Cn� Cp� Cr� Ct� Cv� Cx� Cz� C|� C~� C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D�D� D&fD� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY�DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dy��D�&fD�` D���D���D�#3D�I�D��fD���D�#3D�FfD�p Dǳ3D�&fD�c3Dڹ�D��3D��D�L�D��D�ٚ1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�%A�JA�JA�JA�bA�oA�oA�{A�{A�{A�{A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A� �A�-A�/A�1'A�+A�-A�  AȼjA���AȋDA�XA�VA��A��A�ffA�VA� �AƏ\Aŏ\A�oAĬA�bA�%A���A�XA�&�A���A�%A��A�1A�ĜA�x�A��A�r�A�VA�l�A��7A��/A�p�A�jA��TA���A�=qA��jA�-A��#A�hsA�^5A��A��uA��A�C�A�-A��A��7A��A���A�dZA��A���A��A���A�r�A�ȴA�I�A�ĜA��/A�dZA�A|r�Au�wAt��Asx�Ar��Ar��Aq��AnI�Ai�Ahr�Ae�hAdVA`ĜA\=qAZ^5AX9XAS;dAO�TAO&�AK�FAHȴAE��AC/AA�wA@A�A=��A8�A7hsA6v�A4v�A3ƨA3�A2-A1�A0(�A/l�A,�jA+�7A*{A)7LA(�RA'O�A&��A'x�A(v�A)`BA)��A(M�A"�yA"�yA#l�A#dZA$bNA%t�A%��A%��A$z�A$(�A#�hA!t�A�
At�A�FAA;dA�jAz�A��A�A�A��A33A�+A�A\)A��AS�A�A�A�A33A�A��AĜA�9AƨA=qA �A��A	��AZA�
A+A�A�A^5Ap�A�A�yA�A�A�A+A ��A A�A @�"�@���@�O�@��/@�z�@� �@�o@��!@�V@�z�@�1'@�@��m@��@��@��y@��H@�~�@��@�7L@���@��@�I�@�@�=q@���@�hs@�%@���@�7@�X@�&�@�&�@�Ĝ@�j@���@�o@�E�@�J@�V@�F@��#@�Q�@�t�@�K�@�R@��#@���@��@��`@��@�Q�@�dZ@��y@޸R@އ+@�M�@�@�?}@ܣ�@�b@ۥ�@�dZ@�K�@�o@ڇ+@ّh@��@�Ĝ@؋D@���@׍P@�C�@���@�^5@�{@պ^@�hs@�O�@��@Լj@ԃ@� �@ӍP@�33@��@ҟ�@�M�@�E�@ҟ�@��@Ұ!@�$�@��@ѡ�@мj@Ь@�j@�r�@�I�@���@�  @Ͼw@�l�@�V@͙�@�`B@�O�@�O�@�/@̓u@�|�@�ȴ@�=q@�@ə�@��@ȋD@��
@Ǯ@ǝ�@ǍP@�C�@��y@�=q@��@��T@Ł@�/@���@��`@�%@�Q�@���@�~�@�J@���@��@���@��7@��j@��
@��@��@�;d@�C�@��R@��#@�=q@�5?@�`B@���@��@�l�@�K�@�@���@�n�@�ff@�@��@���@��9@�z�@��
@�K�@���@���@���@���@�hs@�O�@���@�Q�@�1@�@�ff@�@�V@��9@���@��@��
@��@��@��\@��@��@��`@�r�@�1'@�b@��;@��@�33@��H@��@��@���@�5?@���@��h@��@�X@�7L@��@���@�Ĝ@�Ĝ@�Ĝ@��@�9X@���@�ƨ@���@�t�@�\)@�;d@�+@�
=@���@�v�@��@��@�?}@���@���@��@�Q�@�(�@��;@���@�33@��@���@�~�@�=q@�@��#@��-@�p�@�X@��@���@��9@���@��D@�(�@��
@���@�;d@�ȴ@�n�@�E�@�$�@���@�Z@�  @���@�33@�
=@��+@�=q@��@��h@�hs@�X@�&�@���@���@��u@��D@�Z@��w@�S�@�
=@��@��\@��@�@��#@���@���@�O�@�V@���@��@��@��9@�+@��+@|Z@uO�@m�h@fff@Y��@O�@G\)@@  @8�u@2��@*��@$��@!x�@O�@%@9X@A�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 A�%A�JA�JA�JA�bA�oA�oA�{A�{A�{A�{A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A� �A�-A�/A�1'A�+A�-A�  AȼjA���AȋDA�XA�VA��A��A�ffA�VA� �AƏ\Aŏ\A�oAĬA�bA�%A���A�XA�&�A���A�%A��A�1A�ĜA�x�A��A�r�A�VA�l�A��7A��/A�p�A�jA��TA���A�=qA��jA�-A��#A�hsA�^5A��A��uA��A�C�A�-A��A��7A��A���A�dZA��A���A��A���A�r�A�ȴA�I�A�ĜA��/A�dZA�A|r�Au�wAt��Asx�Ar��Ar��Aq��AnI�Ai�Ahr�Ae�hAdVA`ĜA\=qAZ^5AX9XAS;dAO�TAO&�AK�FAHȴAE��AC/AA�wA@A�A=��A8�A7hsA6v�A4v�A3ƨA3�A2-A1�A0(�A/l�A,�jA+�7A*{A)7LA(�RA'O�A&��A'x�A(v�A)`BA)��A(M�A"�yA"�yA#l�A#dZA$bNA%t�A%��A%��A$z�A$(�A#�hA!t�A�
At�A�FAA;dA�jAz�A��A�A�A��A33A�+A�A\)A��AS�A�A�A�A33A�A��AĜA�9AƨA=qA �A��A	��AZA�
A+A�A�A^5Ap�A�A�yA�A�A�A+A ��A A�A @�"�@���@�O�@��/@�z�@� �@�o@��!@�V@�z�@�1'@�@��m@��@��@��y@��H@�~�@��@�7L@���@��@�I�@�@�=q@���@�hs@�%@���@�7@�X@�&�@�&�@�Ĝ@�j@���@�o@�E�@�J@�V@�F@��#@�Q�@�t�@�K�@�R@��#@���@��@��`@��@�Q�@�dZ@��y@޸R@އ+@�M�@�@�?}@ܣ�@�b@ۥ�@�dZ@�K�@�o@ڇ+@ّh@��@�Ĝ@؋D@���@׍P@�C�@���@�^5@�{@պ^@�hs@�O�@��@Լj@ԃ@� �@ӍP@�33@��@ҟ�@�M�@�E�@ҟ�@��@Ұ!@�$�@��@ѡ�@мj@Ь@�j@�r�@�I�@���@�  @Ͼw@�l�@�V@͙�@�`B@�O�@�O�@�/@̓u@�|�@�ȴ@�=q@�@ə�@��@ȋD@��
@Ǯ@ǝ�@ǍP@�C�@��y@�=q@��@��T@Ł@�/@���@��`@�%@�Q�@���@�~�@�J@���@��@���@��7@��j@��
@��@��@�;d@�C�@��R@��#@�=q@�5?@�`B@���@��@�l�@�K�@�@���@�n�@�ff@�@��@���@��9@�z�@��
@�K�@���@���@���@���@�hs@�O�@���@�Q�@�1@�@�ff@�@�V@��9@���@��@��
@��@��@��\@��@��@��`@�r�@�1'@�b@��;@��@�33@��H@��@��@���@�5?@���@��h@��@�X@�7L@��@���@�Ĝ@�Ĝ@�Ĝ@��@�9X@���@�ƨ@���@�t�@�\)@�;d@�+@�
=@���@�v�@��@��@�?}@���@���@��@�Q�@�(�@��;@���@�33@��@���@�~�@�=q@�@��#@��-@�p�@�X@��@���@��9@���@��D@�(�@��
@���@�;d@�ȴ@�n�@�E�@�$�@���@�Z@�  @���@�33@�
=@��+@�=q@��@��h@�hs@�X@�&�@���@���@��u@��D@�Z@��w@�S�@�
=@��@��\@��@�@��#@���@���@�O�@�V@���G�O�@��@��9@�+@��+@|Z@uO�@m�h@fff@Y��@O�@G\)@@  @8�u@2��@*��@$��@!x�@O�@%@9X@A�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	�+B	�%B	�+B	�+B	�%B	�%B	�%B	�%B	�%B	�%B	�%B	�+B	�+B	�+B	�+B	�+B	�+B	�+B	�+B	�1B	�1B	�+B	�+B	�+B	�+B	�+B	�+B	�1B	�JB	��B	��B	��B	ŢB	��B
%�B
.B
7LB
A�B
R�B
k�B
��B
��B
��BB�BH�BhsBw�B�B��B�B�?B�XB�wBȴB�TBbB5?BM�B[#BS�BVBZBZBW
BE�B@�B<jBB�BM�B[#B\)BJ�B9XB$�B�B�BVBB��B�
B��B�B�{B�oB��B�uB�1BjBK�B�B
ŢB
�JB
dZB
D�B	�B	�7B	~�B	y�B	x�B	x�B	x�B	v�B	q�B	gmB	[#B	n�B	q�B	hsB	T�B	<jB	1'B	$�B	uB	1B	B��B�B�B�mB�`B�BB�#B�B�B��B��B�`B�fB�mB�B�B�mB�`B�TB�sB�B�B	+B	hB	�B	8RB	S�B	jB	YB	6FB	9XB	J�B	T�B	k�B	�B	�\B	��B	��B	��B	��B	�1B	�B	�B	��B	��B	�!B	�B	�B	��B	��B	��B	�oB	�\B	�DB	�B	{�B	v�B	r�B	p�B	u�B	}�B	�=B	{�B	{�B	�JB	��B	�LB	�B	��B	�hB	�B	x�B	u�B	q�B	m�B	hsB	aHB	_;B	]/B	\)B	[#B	\)B	[#B	ZB	XB	W
B	W
B	VB	W
B	ZB	ZB	[#B	[#B	\)B	\)B	^5B	]/B	\)B	YB	ZB	`BB	cTB	cTB	cTB	e`B	e`B	e`B	m�B	r�B	t�B	v�B	v�B	x�B	}�B	�B	�bB	��B	��B	��B	��B	�B	�B	�!B	�B	�3B	�?B	�?B	�-B	�B	�B	�B	�'B	�3B	�FB	�XB	�^B	�dB	�^B	�^B	�qB	�jB	�jB	�jB	�jB	�jB	�wB	�}B	�}B	��B	B	B	ĜB	ǮB	ǮB	ƨB	ǮB	��B	��B	��B	��B	��B	��B	��B	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�)B	�#B	�5B	�5B	�TB	�`B	�`B	�fB	�`B	�`B	�ZB	�ZB	�ZB	�ZB	�`B	�fB	�`B	�ZB	�`B	�`B	�mB	�sB	�sB	�sB	�mB	�mB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	�B	�B	�B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
  B
  B	��B	��B
B
B
B
B
B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
1B
1B
1B
1B
1B
1B
+B
+B
+B
+B
+B
%B
+B
+B
+B
1B
1B
	7B
1B
	7B
	7B

=B
DB
DB
DB
DB
JB
PB
VB
\B
\B
\B
bB
bB
bB
hB
hB
hB
hB
hB
hB
hB
\B
\B
\B
bB
bB
bB
bB
bB
hB
hB
hB
oB
oB
oB
oB
oB
oB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
#�B
+B
2-B
6FB
;dB
>wB
D�B
J�B
O�B
T�B
ZB
]/B
bNB
gmB
l�B
o�B
r�B
u�B
y�B
|�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 B	�B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�%B	�rB	��B	��B	�zB	��B
%�B
-�B
7#B
A_B
R�B
k[B
�jB
��B
��B�B|BH�BhHBw�B��B��B��B�B�.B�KBȈB�)B4B5BM�BZ�BS�BU�BY�BY�BV�BEuB@VB<@BBbBM�BZ�B[�BJ�B9&B$�B~BeB&B�B��B��B�WB��B�MB�>B�WB�FB�BjQBK�BVB
�tB
�B
d(B
DlB	�[B	�B	~�B	y�B	x�B	x�B	x�B	v�B	qwB	g9B	Z�B	nfB	qsB	h?B	T�B	<8B	0�B	$�B	?B	�B	�B��B�`B�GB�3B�*B�B��B��B��BӿB��B�'B�-B�5B�GB�GB�3B�(B�B�:B�_B�jB	�B	/B	zB	8B	S�B	jEB	X�B	6B	9B	J�B	T�B	kKB	��B	�"B	�EB	�QB	�kB	�SB	��B	��B	��B	�SB	��B	��B	��B	��B	��B	��B	�RB	�4B	� B	�B	��B	{�B	v�B	rtB	phB	u�B	}�B	�B	{�B	{�B	�B	��B	�B	��B	��B	�*B	��B	x�B	u�B	qlB	mTB	h5B	aB	^�B	\�B	[�B	Z�B	[�B	Z�B	Y�B	W�B	V�B	V�B	U�B	V�B	Y�B	Y�B	Z�B	Z�B	[�B	[�B	]�B	\�B	[�B	X�B	Y�B	`B	cB	cB	cB	e!B	e B	e"B	mUB	rsB	tB	v�B	v�B	x�B	}�B	��B	�$B	�JB	�dB	��B	��B	��B	��B	��B	��B	��B	� B	��B	��B	��B	��B	��B	��B	��B	�B	�B	� B	�%B	�B	�B	�2B	�+B	�*B	�,B	�)B	�,B	�6B	�=B	�=B	�IB	�NB	�QB	�_B	�oB	�oB	�iB	�pB	ʁB	ʂB	ˇB	̌B	ˇB	ʂB	ʃB	�|B	ʃB	ʃB	ˇB	̎B	͓B	ϟB	ΚB	ΚB	ϡB	ϟB	ϠB	ѬB	ӺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�"B	�&B	� B	�!B	�B	�B	�B	�B	�!B	�%B	�!B	�B	�B	� B	�.B	�3B	�3B	�3B	�-B	�.B	�3B	�7B	�:B	�?B	�HB	�FB	�GB	�@B	�FB	�LB	�YB	�tB	�oB	��B	�rB	�pB	�kB	�qB	�tB	��B	��B	�pB	�kB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
�B
 �B
 �B
 �B	��B	��B	��B	��B
 �B
�B
�B
�B
 �B	��B	��B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	�B
B
B
B
B

B
B
B
B
B
B
"B
!B
 B
&B
'B
'B
&B
&B
'B
'B
B
B
B
!B
 B
$B
 B
B
&B
%B
'B
0B
.B
,B
-B
-B
.B
9B
GB
GB
GB
GB
DB
DB
EB
LB
JB
OB
QB
RG�O�B
eB
#�B
*�B
1�B
6B
;$B
>6B
D]B
JB
O�B
T�B
Y�B
\�B
bB
g+B
lJB
o^B
roB
u�B
y�B
|�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.5 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             201904051008002019040510080020190405100800  AO  ARCAADJP                                                                    20181121125955    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181121125955  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181121125955  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190405100800  IP                  G�O�G�O�G�O�                