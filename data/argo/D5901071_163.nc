CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:36Z UW 3.1 conversion   
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  A<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  C$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  TD   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ]�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ed   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  gL   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  n�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  v�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  xl   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �8   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �H   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �L   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        �\   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        �`   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �d   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �hArgo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               �A   AO  20111130142851  20190522121827  1727_5046_163                   2C  D   APEX                            2143                            040306                          846 @��Y�Q@1   @��Zy\��@6vȴ9X�c�bM��1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�  @���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8ffB@ffBH  BP  BX  B`  Bh  Bp  Bx  B��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CC�fCF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C��3C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/y�D0  D0� D1  D1� D2  D2y�D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ�fDK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DXy�DX��DYy�DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dc��Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Djy�Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds` Dz�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�  @���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8ffB@ffBH  BP  BX  B`  Bh  Bp  Bx  B��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CC�fCF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C��3C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/y�D0  D0� D1  D1� D2  D2y�D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ�fDK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DXy�DX��DYy�DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dc��Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Djy�Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds` Dz�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�7LA�33A�5?A�9XA�;dA�;dA�9XA�;dA�;dA�A�A�A�A�A�A�+A�bA�  A��A��TA���A�ƨA�A�ĜAξwAκ^AθRAΰ!AήAΣ�AΙ�AΕ�A���AˁA�33AĴ9A���A°!A���A�JA�  A��A���A�ȴA��\A���A��A� �A��jA���A��A� �A��DA�G�A�&�A���A��;A�XA�oA��A���A�E�A��A��mA��A��wA��A�Q�A��uA�;dA���A��A�t�A��A���A���A�(�A�ffA��A�ƨA�O�A���A���A�?}A�-A��;A�hsA��A��uA�(�A���A���A�`BA�&�A�(�A��^A�ffA���A��!A�  A��A���A���A�G�A��A�$�A�JA��+A��A��-A�oA��A��FA��/A��7A�
=A�jA��mA�K�A���A�v�A�|�A�n�A�p�A��jA�^5A�VA�S�A�I�A�n�A{�AxbNAt �Aq�#Ap  An�DAm�wAm"�Aj��AiAgAd�/AcoAa��A`  A_"�A^r�A]��A]+A[�PAZI�AY��AW��AWS�AV�AUG�AS��AR�AP�/AO7LANbNAM�TAN-ANJAMp�AM"�ALĜAL��ALbAK��AKdZAI�FAG��AE�FAE�AD��ACt�AAƨAAXA@ZA?�A>E�A<~�A;7LA:A�A8��A8-A7�#A7oA5G�A4ffA3�mA3\)A1�;A/�
A-��A-33A*��A)�A'�TA&A�A#oA ffAbNA��A�A��A^5A7LAM�At�AZAC�A`BA��AbNA5?AJAK�A��Ap�A;dAVA
��A
Q�A	�TA	G�A��A�RA�DA��A�hA33AA��A{A��AAz�A7LA n�@�5?@�"�@���@��@��u@�v�@�@���@�D@�A�@�\)@�R@�M�@�^@�33@�@���@�7@��@�9X@�V@�&�@�P@�V@�u@�Z@�%@�o@�x�@ܛ�@�z�@� �@�-@���@�v�@�X@�+@�x�@�1@���@��@϶F@�S�@�ȴ@��#@��@��@ˍP@�M�@ɲ-@ɉ7@ȼj@��;@Ɨ�@�J@�p�@��@Ĵ9@��@�t�@°!@�^5@��@�Z@�S�@���@���@�?}@�?}@�7L@��/@��D@��u@�%@��`@�/@�V@�X@���@���@�@�O�@�`B@�x�@�x�@�hs@�&�@���@��@���@��@��m@���@�l�@�K�@�+@�J@��@�1'@��
@�33@��H@�V@�&�@��9@��@��@�t�@�t�@��@�t�@�;d@�S�@�K�@��@�=q@���@��@�"�@���@��R@���@���@�?}@��@�Z@�bN@��D@��9@���@��@�(�@���@��y@�ff@��#@���@��h@�hs@�x�@��@��@���@�G�@�\)@�{@�5?@�$�@�\)@�1@�Ĝ@�1'@��@��m@��F@�t�@��+@��@�
=@�;d@�dZ@�l�@���@�/@��@�K�@�=q@�X@�%@�V@�&�@�I�@�X@�  @�t�@�
=@��R@��+@�n�@�ff@��T@�&�@�7L@�V@�Q�@���@���@��P@�l�@�K�@�K�@�S�@�S�@��H@�-@��7@�&�@���@���@�/@��@���@���@��@��D@��m@��@��F@�|�@�\)@�33@��@��R@��+@���@�V@���@�A�@��@�  @�ƨ@��@�;d@��y@���@�n�@�E�@�^5@�^5@�5?@�J@�@��h@�?}@�7L@�7L@��@��`@��u@��D@���@�l�@�;d@��y@���@�-@��h@��@�ff1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�7LA�33A�5?A�9XA�;dA�;dA�9XA�;dA�;dA�A�A�A�A�A�A�+A�bA�  A��A��TA���A�ƨA�A�ĜAξwAκ^AθRAΰ!AήAΣ�AΙ�AΕ�A���AˁA�33AĴ9A���A°!A���A�JA�  A��A���A�ȴA��\A���A��A� �A��jA���A��A� �A��DA�G�A�&�A���A��;A�XA�oA��A���A�E�A��A��mA��A��wA��A�Q�A��uA�;dA���A��A�t�A��A���A���A�(�A�ffA��A�ƨA�O�A���A���A�?}A�-A��;A�hsA��A��uA�(�A���A���A�`BA�&�A�(�A��^A�ffA���A��!A�  A��A���A���A�G�A��A�$�A�JA��+A��A��-A�oA��A��FA��/A��7A�
=A�jA��mA�K�A���A�v�A�|�A�n�A�p�A��jA�^5A�VA�S�A�I�A�n�A{�AxbNAt �Aq�#Ap  An�DAm�wAm"�Aj��AiAgAd�/AcoAa��A`  A_"�A^r�A]��A]+A[�PAZI�AY��AW��AWS�AV�AUG�AS��AR�AP�/AO7LANbNAM�TAN-ANJAMp�AM"�ALĜAL��ALbAK��AKdZAI�FAG��AE�FAE�AD��ACt�AAƨAAXA@ZA?�A>E�A<~�A;7LA:A�A8��A8-A7�#A7oA5G�A4ffA3�mA3\)A1�;A/�
A-��A-33A*��A)�A'�TA&A�A#oA ffAbNA��A�A��A^5A7LAM�At�AZAC�A`BA��AbNA5?AJAK�A��Ap�A;dAVA
��A
Q�A	�TA	G�A��A�RA�DA��A�hA33AA��A{A��AAz�A7LA n�@�5?@�"�@���@��@��u@�v�@�@���@�D@�A�@�\)@�R@�M�@�^@�33@�@���@�7@��@�9X@�V@�&�@�P@�V@�u@�Z@�%@�o@�x�@ܛ�@�z�@� �@�-@���@�v�@�X@�+@�x�@�1@���@��@϶F@�S�@�ȴ@��#@��@��@ˍP@�M�@ɲ-@ɉ7@ȼj@��;@Ɨ�@�J@�p�@��@Ĵ9@��@�t�@°!@�^5@��@�Z@�S�@���@���@�?}@�?}@�7L@��/@��D@��u@�%@��`@�/@�V@�X@���@���@�@�O�@�`B@�x�@�x�@�hs@�&�@���@��@���@��@��m@���@�l�@�K�@�+@�J@��@�1'@��
@�33@��H@�V@�&�@��9@��@��@�t�@�t�@��@�t�@�;d@�S�@�K�@��@�=q@���@��@�"�@���@��R@���@���@�?}@��@�Z@�bN@��D@��9@���@��@�(�@���@��y@�ff@��#@���@��h@�hs@�x�@��@��@���@�G�@�\)@�{@�5?@�$�@�\)@�1@�Ĝ@�1'@��@��m@��F@�t�@��+@��@�
=@�;d@�dZ@�l�@���@�/@��@�K�@�=q@�X@�%@�V@�&�@�I�@�X@�  @�t�@�
=@��R@��+@�n�@�ff@��T@�&�@�7L@�V@�Q�@���@���@��P@�l�@�K�@�K�@�S�@�S�@��H@�-@��7@�&�@���@���@�/@��@���@���@��@��D@��m@��@��F@�|�@�\)@�33@��@��R@��+@���@�V@���@�A�@��@�  @�ƨ@��@�;d@��y@���@�n�@�E�@�^5@�^5@�5?@�J@�@��h@�?}@�7L@�7L@��@��`@��u@��D@���@�l�@�;d@��y@���@�-@��h@��@�ff1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBk�Bk�Bk�Bk�Bk�Bk�Bl�Bk�Bl�Bk�Bk�Bk�BjBjBiyBiyBiyBhsBhsBhsBhsBhsBhsBhsBhsBhsBhsBgmBffBbNBH�B,B)�B$�B%�B)�B6FB=qB>wBS�BhsBo�Bw�B|�B�1B�hB��B��B��B��B��B�B�?B�'B�!B�'B�!B�!B�!B�3B�3B�3B�9B�?B�FB�jB�jB��BŢBɺB��B��B��B��B��BǮBÖB�}B�^B�9B�'B�B�B��B�bB�DB�B{�Bm�BZBJ�B@�B49B+B�BuBJB  B  B��B��B�B�ZB��B��BB�RB�?B��B�hB�B�By�Br�Bl�BS�B&�BB
�BB
��B
�!B
��B
��B
�uB
x�B
cTB
8RB
oB	��B	�sB	�/B	��B	ȴB	ĜB	��B	�?B	�B	��B	�oB	�7B	�B	{�B	x�B	v�B	s�B	p�B	l�B	dZB	_;B	ZB	XB	R�B	J�B	C�B	7LB	33B	0!B	.B	49B	:^B	;dB	7LB	8RB	7LB	6FB	5?B	33B	-B	$�B	�B	hB	DB	%B��B��B�B�B�fB�;B�B��B��BǮBŢBÖB�qB�RB�FB�3B�'B�B��B��B��B��B��B��B�uB�PB�7B�%B~�B{�Bu�Bt�Br�Br�Bt�Bx�Br�Bw�Bw�Bv�Bu�Bt�Bs�Bv�Bw�Bx�Bw�Bx�B{�B{�B|�B}�B|�B{�B{�B{�B{�B|�B{�Bx�B� B�B� B~�B}�B}�B�%B�+B�1B�=B�DB�=B�1B�1B�7B�DB�JB�JB�DB�DB�VB�bB�oB�bB�\B�PB�JB�7B�%B�B�B�=B�\B�JB�DB�=B�+B� B�B�B�B~�B� B�B�+B�\B��B��B��B��B��B�B�9B�RB�jBĜBĜBĜBȴB��B��B��B��B��B��B�B�)B�;B�ZB�yB�B�B�B�B�B��B��B	  B	+B	PB	�B	�B	%�B	'�B	+B	0!B	1'B	2-B	49B	5?B	6FB	;dB	A�B	A�B	A�B	@�B	@�B	@�B	@�B	A�B	?}B	;dB	7LB	9XB	;dB	<jB	;dB	;dB	=qB	@�B	?}B	@�B	@�B	@�B	A�B	A�B	B�B	B�B	A�B	A�B	@�B	?}B	?}B	?}B	@�B	C�B	C�B	A�B	A�B	B�B	G�B	N�B	P�B	Q�B	W
B	YB	[#B	\)B	[#B	\)B	[#B	\)B	]/B	^5B	_;B	_;B	aHB	cTB	bNB	]/B	\)B	dZB	jB	v�B	|�B	� B	�B	�1B	�1B	�+B	�+B	�1B	�PB	�bB	�oB	��B	��B	��B	�{B	�uB	�uB	�oB	�hB	�hB	�uB	�{B	�bB	�1B	�B	�+B	�+B	�%B	�+B	�1B	�=B	�=B	�DB	�\B	�\B	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�9B	�FB	�XB	�jB	�jB	�jB	�dB	�^B	�XB	�wB	��B	B	ÖB	ÖB	B	B	B	B	B	��B	��B	��B	B	B	ÖB	ŢB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�)B	�/B	�T1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Bk�Bk�Bk�Bk�Bk�Bk�Bl�Bk�Bl�Bk�Bk�Bk�Bk�BjBiyBiyBiyBhsBhsBhsBhsBhsBhsBhsBhsBhsBhsBgmBgmBgmBXB1'B-B(�B)�B1'B;dB?}BC�BYBn�Br�By�B� B�=B�oB��B��B��B��B��B�!B�RB�3B�'B�-B�'B�-B�-B�3B�3B�9B�?B�FB�^B�wB��BĜBȴB��B��B��B��B��B��BȴBŢBB�wB�FB�'B�!B�!B��B�oB�PB�%B� Bs�B`BBO�BB�B6FB0!B$�B�BhBBB��B��B��B�B�B��BɺB�jB�jB�B��B�+B�B|�Bt�Bq�B^5B,B
=B
�mB
ĜB
�3B
��B
��B
��B
}�B
o�B
E�B
�B
%B	�B	�NB	��B	��B	ƨB	ƨB	�^B	�'B	��B	��B	�PB	�+B	~�B	z�B	x�B	u�B	u�B	p�B	gmB	dZB	\)B	\)B	VB	N�B	G�B	:^B	7LB	2-B	/B	33B	:^B	=qB	8RB	9XB	8RB	7LB	5?B	5?B	2-B	+B	�B	uB	PB	
=B	B��B��B�B�yB�`B�5B�
B��BɺBƨBƨBÖB�dB�RB�?B�FB�-B�B��B��B��B��B��B��B�bB�JB�7B�B�Bx�Bw�Bu�Bu�Bx�B|�Bx�Bz�Bx�Bw�Bv�Bw�Bx�Bx�Bx�By�Bx�Bz�B}�B~�B~�B~�B}�B� B�B|�B|�B}�B~�Bz�B�B�B�B�B�B�B�+B�1B�7B�JB�JB�DB�7B�1B�=B�JB�PB�PB�VB�PB�VB�hB�uB�hB�oB�\B�\B�PB�+B�B�%B�PB�oB�PB�DB�DB�=B� B�%B�B�%B�B�B�%B�+B�VB��B��B��B��B��B�B�FB�XB�jBŢBƨBƨBɺB��B��B��B��B��B�B�#B�5B�HB�fB�B�B�B�B�B��B��B��B	  B	+B	PB	�B	�B	%�B	'�B	+B	1'B	1'B	2-B	49B	5?B	7LB	<jB	A�B	A�B	B�B	@�B	@�B	A�B	@�B	A�B	A�B	>wB	7LB	:^B	<jB	=qB	<jB	=qB	>wB	@�B	A�B	@�B	@�B	@�B	A�B	A�B	B�B	B�B	B�B	C�B	A�B	A�B	?}B	@�B	@�B	C�B	E�B	B�B	A�B	C�B	G�B	N�B	P�B	Q�B	W
B	ZB	\)B	]/B	\)B	]/B	\)B	\)B	]/B	^5B	`BB	_;B	`BB	dZB	ffB	_;B	\)B	dZB	hsB	u�B	{�B	�B	�B	�1B	�1B	�1B	�7B	�1B	�PB	�bB	�oB	��B	��B	��B	��B	�{B	�uB	�{B	�oB	�hB	�uB	��B	�{B	�=B	�%B	�1B	�+B	�%B	�+B	�1B	�DB	�DB	�DB	�\B	�bB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�9B	�FB	�XB	�jB	�jB	�jB	�dB	�dB	�XB	�wB	��B	B	ÖB	ĜB	ÖB	ÖB	ÖB	ĜB	ÖB	��B	��B	��B	ÖB	ÖB	ĜB	ƨB	ǮB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�#B	�#B	�/B	�/B	�T1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<u<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<T��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0 dbar.                                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447312012010314473120120103144731  AO  ARGQ                                                                        20111130142851  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130142851  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144731  IP                  G�O�G�O�G�O�                