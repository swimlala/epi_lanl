CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2016-07-30T18:01:28Z AOML 3.0 creation; 2016-08-07T21:51:32Z UW 3.1 conversion     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7    PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7X   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
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
_FillValue                  0  �|   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �(   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �8   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �<   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �L   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �P   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �T   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �XArgo profile    3.1 1.2 19500101000000  20160730180128  20160825183417  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  5287_9017_136                   2C  D   APEX                            6529                            072314                          846 @׿k��+1   @׿l����@0�dZ��d�$�/1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @@  @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  BffB   B(  B0  B8  B@  BH  BP  BX  B`  Bh��Bn  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  C   C  C  C  C�fC
�C  C  C  C  C  C  C  C  C  C  C   C"�C$�C%�fC(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fDfD� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dy� D�  D�I�D���D�� D�  D�33D���D�� D�  D�FfD�� D�� D��D�P Dڃ3D���D� D�33D�vfD�\�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @`  @�  @�  A  A(  AH  Ah  A�  A�  A�  A�  A�  A�  A�  A�  B  B
  B  BffB"  B*  B2  B:  BB  BJ  BR  BZ  Bb  Bj��Bp  Bz  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  C � C� C� C� CffC
��C� C� C� C� C� C� C� C� C� C� C � C"��C$��C&ffC(� C*� C,� C.� C0� C2� C4� C6� C8� C:� C<� C>� C@� CB� CD� CF� CH� CJ� CL� CN� CP� CR� CT� CV� CX� CZ� C\� C^� C`� Cb� Cd� Cf� Ch� Cj� Cl� Cn� Cp� Cr� Ct� Cv� Cx� Cz� C|� C~� C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�33C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD&fD� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du�Dy� D� D�Y�D���D�� D�0 D�C3D���D�� D� D�VfD�� D�� D���D�` Dړ3D���D�  D�C3D�fD�l�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�$�A�$�A�&�A�&�A�(�A�&�A�&�A�(�A�(�A�+A�-A�+A�(�A�(�A�(�A�+A�-A�/A�1'A�33A�1'A�33A�33A�5?A�(�A�5?A�"�A�1A���A��A�=qA��/A�ƨA֣�A�t�A�;dA�ffAӏ\AҬA�ZAѼjA�;dA�Aδ9A�+A�
=A͋DA̙�A��A��
A�A˼jA˩�A�|�A�VA�A�I�A���A�ƨA�I�A�ĜA�/A�z�Aś�A��A�XA�=qA�/A���A�Q�A�x�A�^5A���A�1'A���A�{A���A���A�ZA�oA�M�A���A�$�A��\A��PA�l�A��!A��A�l�A�9XA��
A�K�A�ƨA�dZA��A�oA�x�A�M�A��yA�bNA���A��A�;dA���A��yA�bA��A�bNA��A|1AzjAy�Av �An��AmXAk�#Ah�uAf-AeG�Ad�9Ac��Ab �A`A�A[C�AXE�AV5?AT��AQ%AOS�AM�^AJ��AI�AG�wAF-AD�9AB~�AA��AA?}A?�wA>5?A=�7A<ffA:M�A9+A6�9A5/A4z�A49XA3��A3A2�DA1��A1�#A0��A/�A.�uA-7LA,{A*�A)XA(�yA(�9A(r�A(ZA(-A'�wA'l�A&�DA$bA"��A"��A!?}A ffA Q�AAp�A��A��AC�AS�A�HAXA�/Ar�A�^A�+Al�A�hA1'Al�A�HA^5A�wA%A9XA�jA�jAdZA7LA�
A�A
�A�A�-A33AffAbNA��A�A �RA �u@��P@���@��@�A�@��@���@���@���@�"�@���@���@�/@��P@�p�@���@�Ĝ@�j@��@���@�K�@��@�  @���@��H@�@��#@��@� �@�@�\)@�o@�
=@���@��y@��@�5?@���@�x�@��@��@��@��;@�o@��@�!@���@�x�@�V@��@��H@�ȴ@��@��`@蛦@�j@�bN@�I�@�P@�"�@��@�@�hs@�X@��@� �@�w@�+@�!@ᙚ@�V@�@�z�@�A�@߅@�@�J@�5?@�-@�V@���@�?}@��`@�z�@�A�@�b@ە�@�K�@�o@���@�E�@٩�@�G�@��@؋D@��@�;d@�~�@�n�@�V@�=q@�5?@�J@��T@Չ7@�/@Լj@ԣ�@� �@�ƨ@�t�@���@�=q@�{@ѡ�@с@�7L@���@ЋD@ЋD@Гu@�bN@Ϯ@�o@��y@���@Χ�@�ff@��@���@�O�@��`@̃@�  @� �@�I�@�Z@�Z@�b@��m@˶F@�t�@��@�n�@�p�@�`B@�%@ȓu@�Q�@�9X@�1@Ǯ@�33@�"�@�ȴ@�hs@î@�@���@�&�@��@��u@�j@��w@�t�@��H@�o@�K�@�S�@�C�@�o@��y@�v�@���@�7L@�%@��j@�j@��@��@�dZ@�S�@�K�@��@�~�@���@���@�X@��@���@���@�"�@��+@��T@���@�X@�G�@��^@��^@�`B@��/@��@�"�@��\@�5?@�J@��@�O�@��@���@�1@���@�"�@�@��\@�$�@���@���@��@���@�(�@��@�ƨ@�t�@�33@�o@��@���@�V@�-@��h@�/@���@��j@�z�@�9X@�  @�ƨ@�C�@���@��!@��\@�n�@�E�@�E�@�@���@�p�@�&�@���@���@�Z@�1'@��@���@�t�@�"�@��!@�ff@�=q@�{@���@���@�@��-@�x�@�`B@�?}@�%@��@���@�j@�(�@��@��@�l�@�C�@���@�@��^@��@�I�@���@���@��@{t�@qx�@hr�@]?}@V��@N��@D��@;�
@6{@.�y@)X@$�@ b@�
@�R@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 A�$�A�$�A�&�A�&�A�(�A�&�A�&�A�(�A�(�A�+A�-A�+A�(�A�(�A�(�A�+A�-A�/A�1'A�33A�1'A�33A�33A�5?A�(�A�5?A�"�A�1A���A��A�=qA��/A�ƨA֣�A�t�A�;dA�ffAӏ\AҬA�ZAѼjA�;dA�Aδ9A�+A�
=A͋DA̙�A��A��
A�A˼jA˩�A�|�A�VA�A�I�A���A�ƨA�I�A�ĜA�/A�z�Aś�A��A�XA�=qA�/A���A�Q�A�x�A�^5A���A�1'A���A�{A���A���A�ZA�oA�M�A���A�$�A��\A��PA�l�A��!A��A�l�A�9XA��
A�K�A�ƨA�dZA��A�oA�x�A�M�A��yA�bNA���A��A�;dA���A��yA�bA��A�bNA��A|1AzjAy�Av �An��AmXAk�#Ah�uAf-AeG�Ad�9Ac��Ab �A`A�A[C�AXE�AV5?AT��AQ%AOS�AM�^AJ��AI�AG�wAF-AD�9AB~�AA��AA?}A?�wA>5?A=�7A<ffA:M�A9+A6�9A5/A4z�A49XA3��A3A2�DA1��A1�#A0��A/�A.�uA-7LA,{A*�A)XA(�yA(�9A(r�A(ZA(-A'�wA'l�A&�DA$bA"��A"��A!?}A ffA Q�AAp�A��A��AC�AS�A�HAXA�/Ar�A�^A�+Al�A�hA1'Al�A�HA^5A�wA%A9XA�jA�jAdZA7LA�
A�A
�A�A�-A33AffAbNA��A�A �RA �u@��P@���@��@�A�@��@���@���@���@�"�@���@���@�/@��P@�p�@���@�Ĝ@�j@��@���@�K�@��@�  @���@��H@�@��#@��@� �@�@�\)@�o@�
=@���@��y@��@�5?@���@�x�@��@��@��@��;@�o@��@�!@���@�x�@�V@��@��H@�ȴ@��@��`@蛦@�j@�bN@�I�@�P@�"�@��@�@�hs@�X@��@� �@�w@�+@�!@ᙚ@�V@�@�z�@�A�@߅@�@�J@�5?@�-@�V@���@�?}@��`@�z�@�A�@�b@ە�@�K�@�o@���@�E�@٩�@�G�@��@؋D@��@�;d@�~�@�n�@�V@�=q@�5?@�J@��T@Չ7@�/@Լj@ԣ�@� �@�ƨ@�t�@���@�=q@�{@ѡ�@с@�7L@���@ЋD@ЋD@Гu@�bN@Ϯ@�o@��y@���@Χ�@�ff@��@���@�O�@��`@̃@�  @� �@�I�@�Z@�Z@�b@��m@˶F@�t�@��@�n�@�p�@�`B@�%@ȓu@�Q�@�9X@�1@Ǯ@�33@�"�@�ȴ@�hs@î@�@���@�&�@��@��u@�j@��w@�t�@��H@�o@�K�@�S�@�C�@�o@��y@�v�@���@�7L@�%@��j@�j@��@��@�dZ@�S�@�K�@��@�~�@���@���@�X@��@���@���@�"�@��+@��T@���@�X@�G�@��^@��^@�`B@��/@��@�"�@��\@�5?@�J@��@�O�@��@���@�1@���@�"�@�@��\@�$�@���@���@��@���@�(�@��@�ƨ@�t�@�33@�o@��@���@�V@�-@��h@�/@���@��j@�z�@�9X@�  @�ƨ@�C�@���@��!@��\@�n�@�E�@�E�@�@���@�p�@�&�@���@���@�Z@�1'@��@���@�t�@�"�@��!@�ff@�=q@�{@���@���@�@��-@�x�@�`B@�?}@�%@��@���@�j@�(�@��@��@�l�@�C�@���@�G�O�@��@�I�@���@���@��@{t�@qx�@hr�@]?}@V��@N��@D��@;�
@6{@.�y@)X@$�@ b@�
@�R@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
9XB
:^B
:^B
:^B
:^B
:^B
:^B
:^B
:^B
:^B
:^B
:^B
;dB
;dB
;dB
;dB
;dB
;dB
:^B
:^B
;dB
;dB
;dB
;dB
=qB
;dB
=qB
<jB
<jB
1'B
�B
�B
'�B
+B
.B
49B
:^B
E�B
L�B
Q�B
W
B
S�B
bNB
}�B
�{B
��B
�\B
}�B
�B
�oB
��B
�B
�LB
��B
ǮB
�BB �B,B2-BG�B`BBw�B�hB�B�wB��B�)B�fBB	7B�BbB��BhB#�BE�B;dB�B�B/B(�B%�B�BVB�yB��B��B�9B��B�\By�BbNBW
B!�BVBB
�B
�#B
�B
��B
ǮB
��B
��B
�hB
�B
e`B
P�B
49B
�B
%B	��B	�mB	��B	��B	��B	�uB	�B	v�B	s�B	p�B	r�B	n�B	cTB	O�B	D�B	;dB	49B	,B	-B	)�B	�B	 �B	!�B	�B	�B	hB	PB		7B	B��B��B��B�B�B�ZB�fB�yB�B�B�B�B�B�B��B	%B	1B	oB	�B	!�B	+B	,B	-B	.B	.B	/B	2-B	33B	33B	/B	&�B	'�B	"�B	�B	�B	�B	�B	�B	�B	�B	 �B	�B	�B	�B	�B	�B	�B	{B	{B	�B	�B	�B	�B	�B	"�B	0!B	.B	+B	"�B	)�B	:^B	?}B	9XB	/B	(�B	%�B	!�B	�B	"�B	�B	{B	�B	�B	�B	�B	)�B	D�B	F�B	<jB	=qB	W
B	ZB	e`B	cTB	cTB	[#B	YB	^5B	v�B	~�B	�+B	�%B	�PB	�\B	�bB	�bB	�bB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�'B	�?B	�FB	�LB	�XB	��B	ƨB	ƨB	ĜB	ŢB	ƨB	ŢB	ɺB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�#B	�)B	�)B	�)B	�/B	�/B	�/B	�5B	�;B	�;B	�;B	�;B	�BB	�HB	�HB	�HB	�NB	�HB	�HB	�NB	�NB	�NB	�NB	�TB	�TB	�ZB	�fB	�sB	�sB	�yB	�yB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�mB	�`B	�`B	�fB	�fB	�fB	�fB	�mB	�sB	�yB	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
  B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B	��B	��B	��B	��B	��B	��B
  B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
1B
	7B
	7B
	7B
	7B
	7B
DB
DB
DB
DB
JB
JB
JB
JB
JB
DB
DB
DB
DB
DB
DB

=B

=B
JB
VB
\B
uB
�B
#�B
+B
0!B
5?B
<jB
?}B
G�B
N�B
W
B
ZB
_;B
dZB
iyB
m�B
q�B
u�B
y�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 B
96B
:;B
:>B
:>B
:@B
:@B
:@B
:>B
:>B
:>B
:@B
:@B
;EB
;EB
;EB
;FB
;EB
;EB
:@B
:BB
;BB
;DB
;DB
;EB
=PB
;HB
=SB
<JB
<KB
1	B
zB
�B
'�B
*�B
-�B
4B
:=B
E�B
L�B
Q�B
V�B
S�B
b+B
}�B
�[B
�ZB
�8B
}�B
��B
�MB
��B
��B
�'B
�[B
ǉB
�B �B+�B2BG�B`Bw�B�?B��B�MBʗB��B�9B �B	
BUB6B��B<B#�BEvB;5B�B�B.�B(�B%�B�B(B�KBиB�SB�B��B�+By�Bb BV�B!�B+B�B
�SB
��B
��B
��B
ǂB
��B
�sB
�<B
��B
e1B
P�B
4B
zB
�B	��B	�DB	��B	��B	��B	�MB	��B	v�B	s�B	p{B	r�B	npB	c,B	O�B	DvB	;>B	4B	+�B	,�B	)�B	�B	 �B	!�B	�B	kB	AB	)B		B	�B��B��B��B�yB�YB�4B�>B�RB�bB�VB�hB�wB�B�B��B	�B		B	CB	[B	!�B	*�B	+�B	,�B	-�B	-�B	.�B	2B	3B	3	B	.�B	&�B	'�B	"�B	�B	{B	pB	tB	�B	|B	nB	 �B	�B	�B	hB	VB	VB	TB	RB	OB	WB	[B	`B	fB	tB	"�B	/�B	-�B	*�B	"�B	)�B	:1B	?PB	9+B	.�B	(�B	%�B	!�B	lB	"�B	kB	NB	SB	cB	XB	cB	)�B	DkB	FxB	<9B	=BB	V�B	Y�B	e/B	c"B	c$B	Z�B	X�B	^B	v�B	~�B	��B	��B	�B	�'B	�1B	�1B	�0B	�HB	�aB	�yB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�$B	�TB	�sB	�sB	�eB	�mB	�tB	�lB	ɆB	�}B	ɃB	ʊB	ʋB	ˏB	ˑB	ˑB	̗B	͞B	ЮB	ҽB	��B	��B	һB	ѶB	аB	ϦB	ΣB	ΣB	ϨB	ѶB	��B	��B	��B	��B	ѷB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�.B	�>B	�;B	�BB	�AB	�:B	�<B	�AB	�GB	�EB	�NB	�OB	�VB	�YB	�WB	�ZB	�ZB	�[B	�aB	�fB	�gB	�gB	�fB	�gB	�ZB	�TB	�TB	�MB	�TB	�`B	�eB	�gB	�eB	�fB	�fB	�gB	�eB	�jB	�eB	�YB	�aB	�cB	�`B	�`B	�ZB	�aB	�_B	�[B	�\B	�KB	�7B	�(B	�(B	�/B	�-B	�/B	�/B	�6B	�<B	�CB	�DB	�sB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B	��B	��B	��B	��B	��B	��B	��B	��B	��B
�B
 �B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B

B
B

B
B
B
B
B
B
B

B
B

B
B

B
B

B

G�O�B
B
B
:B
XB
#�B
*�B
/�B
5B
<,B
?@B
GtB
N�B
V�B
Y�B
^�B
dB
i=B
mUB
qoB
u�B
y�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.5 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451322016080714513220160807145132  AO  ARCAADJP                                                                    20160730180128    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160730180128  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160730180128  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145132  IP                  G�O�G�O�G�O�                