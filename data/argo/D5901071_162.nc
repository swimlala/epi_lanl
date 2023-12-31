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
_FillValue                 �  A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  K|   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  Mx   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  Uh   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]X   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  _T   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  gD   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  i@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  q0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  y    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  {   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �(   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �(   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �(   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �(   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �T   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �X   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �\   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �`   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �d   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               �A   AO  20111130142838  20190522121827  1727_5046_162                   2C  D   APEX                            2143                            040306                          846 @��+���1   @�����@6G+I��c�t�j~�1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  BffB  B   B(  B0  B8  B@  BH  BP  BX��B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP�CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D/��D0� D1  D1� D2  D2� D3  D3�fD4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH�fDI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df�fDg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dy�fD�)�D�l�D���D��3D��D�i�D���D���D��D�Y�D��3D�ٚD�33D�l�Dړ3D��3D�#3D�L�D� D��f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  BffB  B   B(  B0  B8  B@  BH  BP  BX��B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP�CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D/��D0� D1  D1� D2  D2� D3  D3�fD4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH�fDI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df�fDg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dy�fD�)�D�l�D���D��3D��D�i�D���D���D��D�Y�D��3D�ٚD�33D�l�Dړ3D��3D�#3D�L�D� D��f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AρAχ+Aω7AϋDAω7AϋDAύPAϏ\AϓuAϓuAϓuAω7Aω7Aω7Aω7AϋDAύPAύPAϗ�Aω7A�l�A�^5A�=qA���Aͧ�A�ȴA���A�ƨAǮA��yA��Aá�A��A®A�t�A�&�A�7LA��7A�ȴA�~�A���A�VA��#A��9A��A�t�A��HA���A�v�A�%A�S�A�$�A�%A��
A��A�G�A�A��A�;dA�/A�-A�bA���A���A�&�A�jA���A���A�r�A�JA���A���A�jA�M�A�A�^5A���A�x�A�ƨA�A���A�9XA��PA�JA�ƨA���A� �A�A�A��hA�{A�/A��hA��yA�(�A��TA���A��A���A�A���A�bNA�dZA��A��FA��A�S�A�{A���A���A�E�A�n�A�$�A�$�A� �A�A��A��jA�7LA�VA��jA�`BA�33A�JA���A��A�{A��TA���A~A�A{;dAy��AxbNAv(�At �ApĜAn�9Am\)AlJAi|�Ah�Ag�AfI�Ae�hAdr�Ac�AaO�A^��A\ffA[XAZ��AZI�AXĜAVZAS��ASAR��AQ�#AP��AO�#AN��AL��ALAIAGXAG%AF�AF�`AF�RAFE�AE
=AB�ABZAA�
AA?}A@�uA>ffA=|�A<�+A;�-A:I�A7�FA6�`A6VA5��A4$�A3VA1�A0-A/\)A.�yA.VA. �A-7LA+t�A+A*^5A)�A)��A(ĜA'�PA&�/A&5?A%hsA$�\A" �A!`BA ��A   A�A�AK�AbNA?}Ar�At�Av�A�hA�jAx�A�A�7A33A�A�A
=AQ�A��A��A�;A
n�A	O�A^5A�A�FA�#A&�A��A��Al�A I�A $�A b@�ƨ@���@��\@�o@���@��@��D@�w@�~�@홚@�`B@�j@��@�9X@�33@�1'@�C�@���@�^5@�p�@ߕ�@��@�I�@�"�@ف@ى7@�hs@���@�Z@�1'@׮@�C�@��H@�$�@ӍP@�j@�33@�+@��@�33@�@��@Ͳ-@�7L@���@��@�{@�n�@���@�J@�E�@�ff@��@�S�@�
=@�o@�o@�
=@��@�Ĝ@̃@�I�@�Q�@���@�\)@�-@ǥ�@Ǯ@�ƨ@Ǯ@�C�@�33@�"�@�t�@Ǿw@�;d@Ɵ�@Ų-@�G�@ċD@�A�@�dZ@��@���@� �@�1@�b@��w@��@�+@��@��@��R@��@��@��`@��D@�9X@�C�@�J@���@�7L@��9@�j@�  @� �@��F@���@�l�@�t�@���@�Q�@�9X@��m@���@��@�7L@�bN@�&�@��m@���@�{@���@���@�hs@�O�@�V@��/@���@�Ĝ@���@�j@�9X@�  @��m@��
@��F@�33@�dZ@�I�@�r�@�K�@��@�;d@�\)@��@�K�@�o@��@�5?@���@���@�hs@���@���@�A�@��m@�+@�o@���@��H@�ȴ@���@�v�@�=q@�J@�`B@���@�Ĝ@�Z@�1'@���@�33@��@��@�x�@�hs@�O�@�?}@��@��`@�A�@��m@��w@���@���@�l�@���@�ff@�$�@�@���@��7@��h@��h@���@���@�G�@��@��@��/@��/@��`@��@��@���@��@�\)@�@���@��\@�E�@��#@���@��^@���@�?}@��D@�I�@�A�@� �@�  @�ƨ@��@���@���@�dZ@�;d@���@�n�@�E�@�5?@�{@�@��@���@�hs@���@���@���@�bN@�A�@���@�+@��@�(�@��@{t�@r��@jM�@dZ@["�@S"�@M/@G�;@?�@7�;@3�
@-�@'�@"M�@S�@5?@��@Z@A�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111AρAχ+Aω7AϋDAω7AϋDAύPAϏ\AϓuAϓuAϓuAω7Aω7Aω7Aω7AϋDAύPAύPAϗ�Aω7A�l�A�^5A�=qA���Aͧ�A�ȴA���A�ƨAǮA��yA��Aá�A��A®A�t�A�&�A�7LA��7A�ȴA�~�A���A�VA��#A��9A��A�t�A��HA���A�v�A�%A�S�A�$�A�%A��
A��A�G�A�A��A�;dA�/A�-A�bA���A���A�&�A�jA���A���A�r�A�JA���A���A�jA�M�A�A�^5A���A�x�A�ƨA�A���A�9XA��PA�JA�ƨA���A� �A�A�A��hA�{A�/A��hA��yA�(�A��TA���A��A���A�A���A�bNA�dZA��A��FA��A�S�A�{A���A���A�E�A�n�A�$�A�$�A� �A�A��A��jA�7LA�VA��jA�`BA�33A�JA���A��A�{A��TA���A~A�A{;dAy��AxbNAv(�At �ApĜAn�9Am\)AlJAi|�Ah�Ag�AfI�Ae�hAdr�Ac�AaO�A^��A\ffA[XAZ��AZI�AXĜAVZAS��ASAR��AQ�#AP��AO�#AN��AL��ALAIAGXAG%AF�AF�`AF�RAFE�AE
=AB�ABZAA�
AA?}A@�uA>ffA=|�A<�+A;�-A:I�A7�FA6�`A6VA5��A4$�A3VA1�A0-A/\)A.�yA.VA. �A-7LA+t�A+A*^5A)�A)��A(ĜA'�PA&�/A&5?A%hsA$�\A" �A!`BA ��A   A�A�AK�AbNA?}Ar�At�Av�A�hA�jAx�A�A�7A33A�A�A
=AQ�A��A��A�;A
n�A	O�A^5A�A�FA�#A&�A��A��Al�A I�A $�A b@�ƨ@���@��\@�o@���@��@��D@�w@�~�@홚@�`B@�j@��@�9X@�33@�1'@�C�@���@�^5@�p�@ߕ�@��@�I�@�"�@ف@ى7@�hs@���@�Z@�1'@׮@�C�@��H@�$�@ӍP@�j@�33@�+@��@�33@�@��@Ͳ-@�7L@���@��@�{@�n�@���@�J@�E�@�ff@��@�S�@�
=@�o@�o@�
=@��@�Ĝ@̃@�I�@�Q�@���@�\)@�-@ǥ�@Ǯ@�ƨ@Ǯ@�C�@�33@�"�@�t�@Ǿw@�;d@Ɵ�@Ų-@�G�@ċD@�A�@�dZ@��@���@� �@�1@�b@��w@��@�+@��@��@��R@��@��@��`@��D@�9X@�C�@�J@���@�7L@��9@�j@�  @� �@��F@���@�l�@�t�@���@�Q�@�9X@��m@���@��@�7L@�bN@�&�@��m@���@�{@���@���@�hs@�O�@�V@��/@���@�Ĝ@���@�j@�9X@�  @��m@��
@��F@�33@�dZ@�I�@�r�@�K�@��@�;d@�\)@��@�K�@�o@��@�5?@���@���@�hs@���@���@�A�@��m@�+@�o@���@��H@�ȴ@���@�v�@�=q@�J@�`B@���@�Ĝ@�Z@�1'@���@�33@��@��@�x�@�hs@�O�@�?}@��@��`@�A�@��m@��w@���@���@�l�@���@�ff@�$�@�@���@��7@��h@��h@���@���@�G�@��@��@��/@��/@��`@��@��@���@��@�\)@�@���@��\@�E�@��#@���@��^@���@�?}@��D@�I�@�A�@� �@�  @�ƨ@��@���@���@�dZ@�;d@���@�n�@�E�@�5?@�{@�@��@���@�hs@���@���@���@�bN@�A�@���@�+@��@�(�@��@{t�@r��@jM�@dZ@["�@S"�@M/@G�;@?�@7�;@3�
@-�@'�@"M�@S�@5?@��@Z@A�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBt�Bt�Bt�Bt�Bt�Bt�Bs�Bt�Bt�Bt�Bs�Br�Br�Br�Br�Br�Br�Br�Bs�Br�Bp�Bo�Bn�BffBS�B5?B,B �B)�B9XB=qB9XBE�BI�BM�BW
BT�B^5BZBVBl�B|�B� B�B�7B�=B�B�B�PB��B��B��B��B��B��B��B��B��B�B�'B�3B�RB�jB��B�qBÖBƨBǮBȴBɺBɺBɺBɺBȴBȴBȴB��BɺB��BȴBǮBȴBÖB��B��B�B��B�}BĜBÖBB��B�^B�?B�XB�}B��B�XB�B��Bw�BcTBM�B8RB)�B$�B�B��B��BȴB�XB��BffB$�BhB
��B
�HB
��B
��B
�RB
�3B
�B
�B
��B
��B
��B
}�B
R�B
+B
�B

=B
  B	�B	�HB	��B	�}B	�LB	�B	��B	��B	�hB	�PB	�1B	�B	z�B	k�B	^5B	R�B	N�B	L�B	J�B	F�B	@�B	8RB	6FB	33B	49B	6FB	6FB	0!B	&�B	�B	�B	hB	hB	hB	hB	bB	PB		7B	B	B��B��B��B�B�B�B�yB�NB�B�
B��B��B��BƨBÖB�}B�dB�^B�RB�FB�-B�B�B��B��B��B��B��B��B��B��B�oB�\B�VB�JB�DB�DB�7B�+B�B�B� B}�Bz�Bx�Bu�Bq�Bn�Bn�Bm�Bp�Bn�Br�Bm�Bm�Bp�Bs�Bq�Bq�Bq�Bn�Bn�Bo�Bp�Bo�Bn�Bp�Bq�Bp�Bo�Bm�Bp�Bo�Bq�Bk�BffBgmBgmBffBe`BiyBiyBk�Bk�Bl�Bn�Bo�Bp�Bp�Bs�Bu�Bu�Bt�Bt�B{�B�+B�7B�VB�bB�bB�hB�uB�oB�bB�PB�=B�DB�{B��B��B��B��B��B��B�B�B�jBǮBƨBɺB��B��B�B�B�/B�5B�5B�5B�5B�5B�/B�BB�mB�yB�B�B�B�B�B�B�B�B�B��B��B	B	+B	DB	JB	bB	bB	hB	hB	PB	DB	DB	
=B	%B��B�yB�TB�/B�/B�5B�BB�NB�ZB�ZB�fB�yB�B�B�B�B��B��B��B��B	  B	B		7B	\B	uB	{B	uB	bB	PB	bB	�B	�B	�B	�B	�B	�B	�B	 �B	"�B	$�B	$�B	%�B	&�B	&�B	&�B	(�B	+B	,B	-B	2-B	9XB	A�B	H�B	K�B	N�B	P�B	R�B	W
B	[#B	]/B	]/B	_;B	bNB	cTB	dZB	gmB	k�B	l�B	m�B	r�B	r�B	r�B	r�B	q�B	r�B	s�B	r�B	q�B	q�B	r�B	s�B	t�B	u�B	t�B	t�B	v�B	{�B	}�B	}�B	}�B	}�B	}�B	~�B	�B	�%B	�+B	�7B	�7B	�JB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�3B	�9B	�LB	�XB	�^B	�dB	�dB	�jB	�wB	��B	��B	��B	ÖB	ŢB	ŢB	ŢB	ŢB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�TB	�B
  B
DB
hB
�B
"�B
,B
2-B
6FB
@�B
F�B
H�B
O�B
VB
[#B
aHB
gmB
l�B
q�B
v�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111Bt�Bt�Bt�Bt�Bt�Bt�Bs�Bt�Bt�Bt�Bs�Br�Br�Br�Br�Br�Br�Br�Bs�Br�Bp�Bo�Bo�BjB_;B8RB.B"�B.B@�B>wB<jBF�BJ�BT�B\)BYBbNB\)B[#Bn�B� B�B�B�\B�PB�B�B�\B��B��B��B��B��B��B��B��B��B�B�'B�9B�dB��BÖB��BƨBȴBȴB��B��B��B��BɺBɺB��B��B��B��B��B��BɺB��BŢB��B��B�)B��BBǮBǮBŢBĜB�wB�FB�^B��BĜB�}B�-B��B}�Bk�BVB>wB,B'�B)�BB�B��B�wB�Bv�B+B�B
=B
�yB
��B
ÖB
�^B
�9B
�!B
�B
�B
��B
��B
�B
[#B
1'B
�B
PB
B	��B	�yB	��B	B	�^B	�9B	��B	��B	�uB	�\B	�DB	�+B	�B	r�B	dZB	VB	P�B	N�B	N�B	M�B	G�B	:^B	7LB	5?B	8RB	8RB	9XB	6FB	)�B	&�B	�B	oB	hB	hB	oB	oB	hB	bB	B	B	B	  B	  B��B�B�B�B�sB�)B�B�
B��B��B��BǮB��B�jB�jB�XB�XB�LB�!B�B�B��B��B��B��B��B��B��B��B�oB�bB�\B�DB�DB�DB�=B�1B�B�B�B}�B{�By�By�Br�Bo�Bq�Bv�Bq�Bu�Br�Bn�Bs�Bx�Bu�Bt�Bt�Bu�Bt�Bq�Bq�Bp�Bq�Bs�Br�Bp�Bp�Bp�Bu�Bt�Bv�Bo�BhsBhsBiyBgmBffBjBn�Bm�Bm�Bq�Bp�Bp�Bq�Br�Bv�Bx�Bx�Bv�Bw�B{�B�+B�=B�\B�bB�hB�oB�{B�uB�{B�hB�DB�DB�uB��B��B��B��B��B��B�B�B�jBȴBƨBɺB��B��B�B�#B�/B�5B�5B�BB�BB�;B�/B�BB�sB�B�B�B�B�B�B�B�B�B�B��B	  B	B	1B	JB	PB	hB	hB	oB	uB	\B	DB	DB	DB	JB��B�B�fB�5B�5B�;B�HB�TB�`B�fB�sB�B�B�B�B��B��B��B��B��B	  B	B		7B	\B	{B	�B	{B	uB	PB	\B	�B	�B	�B	�B	�B	�B	�B	 �B	"�B	$�B	$�B	%�B	&�B	&�B	&�B	(�B	+B	,B	.B	2-B	8RB	A�B	J�B	K�B	N�B	P�B	R�B	W
B	[#B	^5B	^5B	`BB	bNB	cTB	e`B	hsB	l�B	m�B	n�B	r�B	r�B	r�B	r�B	q�B	r�B	s�B	r�B	r�B	r�B	r�B	t�B	t�B	v�B	u�B	u�B	x�B	|�B	}�B	}�B	}�B	}�B	}�B	� B	�B	�%B	�+B	�7B	�7B	�PB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�3B	�9B	�RB	�XB	�^B	�dB	�jB	�qB	�wB	��B	��B	��B	ÖB	ŢB	ŢB	ŢB	ŢB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�TB	�B
B
DB
hB
�B
"�B
,B
2-B
6FB
@�B
G�B
H�B
O�B
VB
[#B
aHB
gmB
l�B
q�B
v�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<T��<#�
<#�
<#�
<#�
<#�
<#�
<T��<49X<#�
<#�
<#�
<49X<�o<#�
<#�
<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0 dbar.                                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447312012010314473120120103144731  AO  ARGQ                                                                        20111130142838  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130142838  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144731  IP                  G�O�G�O�G�O�                