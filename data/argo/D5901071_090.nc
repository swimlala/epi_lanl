CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:16Z UW 3.1 conversion   
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
_FillValue                 �  Kl   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  Mh   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  UP   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]8   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  _4   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  g   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  i   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  q    PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  x�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  z�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �\   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �l   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �p   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               ZA   AO  20111130141212  20190522121826  1727_5046_090                   2C  D   APEX                            2143                            040306                          846 @Ԋx��
1   @Ԋ��@7���R�c� ě�1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A���A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW�fDX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dhy�Dh��Di� Dj  Dj� Dk  Dk� Dl  Dl� Dl��Dmy�Dm��Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� DyٚD�&fD�ffD��3D��fD�  D�p D�� D�ٚD�  D�` D��3D�� D�33D�L�DڦfD�ٚD� D�ffD� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�  @�  A   A   A@  A`  A�  A�  A�  A�  A���A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW�fDX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dhy�Dh��Di� Dj  Dj� Dk  Dk� Dl  Dl� Dl��Dmy�Dm��Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� DyٚD�&fD�ffD��3D��fD�  D�p D�� D�ٚD�  D�` D��3D�� D�33D�L�DڦfD�ٚD� D�ffD� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AոRA�/AԼjA�`BA�;dA�1'A�-A�&�A�"�A� �A��A�VA�A�JA�+A���A�-A��
A��AЛ�A�&�Aϝ�A��A��A�JAʼjA�`BA�Q�AǕ�A�{A� �A�dZA���A��A��A�ȴA��
A���A�dZA��A��A��-A�\)A���A��yA�p�A�jA���A�?}A�5?A��FA��A�
=A�33A���A� �A�A�I�A��yA�hsA��+A��FA�A�A��hA� �A��FA�~�A�^5A�$�A�ĜA��wA�/A��wA�M�A��9A��wA�l�A��A���A�E�A�ȴA���A��uA���A���A���A���A��A�-A���A�G�A��\A�"�A��PA��A�VA��uA��A��#A�A�%A�ƨA��A��A�bA��-A�bNA�|�A��HA�z�A�XA�O�A��A�^5A�bNA��/A��FA��
A�33A�p�A��RA�#A|��A{K�Az�uAyO�Axn�Aw&�Au��Au%At�As�ArbApZAn��Am%Aj�AiS�Ah�Ah�!Ag�hAe��Ac�^Aa��A_�TA^��A^ �A]�-A\r�AZM�AX�DAV�HAUp�AT��ASAQ��AP��AOl�AMdZAL��AL$�AK7LAJ$�AI"�AH��AG��AFZAD��AC��AAA?7LA=�FA<�+A;��A9�A8 �A7��A6�!A5��A4��A4^5A3�
A3O�A2�/A21'A1��A1oA0ȴA05?A/A.��A.-A-�;A-��A,��A+�FA*v�A)O�A(�/A'�7A&M�A%��A$�RA$1A#t�A"JA!ƨA!�A!?}A!�A r�AƨA�uA�A/A�AE�A�-AO�A�Az�A1A�PA�`A�A��AAz�Ax�A9XAx�AffA�wA�7A�+AS�A-A|�A33A�uA�hA
�HA
ffA	p�A�`A��AA�A�-AC�A��A��A�\AVA�A�A=qA�AdZA�jA��A��A �H@���@��@��`@�r�@� �@�hs@���@�dZ@��@��@�P@�%@�33@�j@�J@�@�ȴ@�b@݁@�Z@�K�@ڟ�@ف@ٺ^@�Z@��T@ԃ@�~�@љ�@϶F@Η�@͙�@�7L@�bN@�@ʧ�@�@Ȭ@ǶF@ư!@ũ�@���@öF@�l�@�E�@��7@�G�@�Z@���@���@��R@��h@��m@�l�@��+@�I�@�+@�$�@�Q�@�M�@��@� �@�K�@��!@�^5@��@��#@�7L@�Ĝ@��@��T@��@�(�@��@��P@���@�@���@��@�A�@���@��@��!@�J@�G�@���@�j@�A�@�1'@�(�@�l�@�ȴ@��@���@��@���@��@�r�@�Q�@� �@�1@��m@��;@��w@�|�@�+@�ȴ@���@�^5@�V@�M�@�E�@�-@�5?@�-@�{@�@���@���@���@���@��@��@��D@�9X@��m@���@�
=@���@�n�@��@�&�@�V@��@���@�bN@�bN@�Z@�Z@�I�@�bN@��@���@��u@���@��@�;d@�"�@�E�@��^@�@��7@���@��h@�hs@�?}@���@��9@�(�@�9X@�z�@�z�@�j@�1@���@�o@�+@��H@�^5@��@�X@��@� �@���@���@�X@��@�dZ@�^5@��@�j@�O�@�n�@���@�-@���@�G�@��@��@�%@��@��@��@�1@��P@�C�@�
=@���@���@�M�@�@��#@���@��^@���@��@���@��u@�Z@�Q�@�Q�@�(�@�1@�  @��m@���@��@��P@�
=@��!@���@�v�@�=q@���@�C�@}�@s"�@kƨ@c�
@[�F@S��@L��@C�
@<��@5�@.@'�@!��@��@��@Z@��@9X@+11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  AոRA�/AԼjA�`BA�;dA�1'A�-A�&�A�"�A� �A��A�VA�A�JA�+A���A�-A��
A��AЛ�A�&�Aϝ�A��A��A�JAʼjA�`BA�Q�AǕ�A�{A� �A�dZA���A��A��A�ȴA��
A���A�dZA��A��A��-A�\)A���A��yA�p�A�jA���A�?}A�5?A��FA��A�
=A�33A���A� �A�A�I�A��yA�hsA��+A��FA�A�A��hA� �A��FA�~�A�^5A�$�A�ĜA��wA�/A��wA�M�A��9A��wA�l�A��A���A�E�A�ȴA���A��uA���A���A���A���A��A�-A���A�G�A��\A�"�A��PA��A�VA��uA��A��#A�A�%A�ƨA��A��A�bA��-A�bNA�|�A��HA�z�A�XA�O�A��A�^5A�bNA��/A��FA��
A�33A�p�A��RA�#A|��A{K�Az�uAyO�Axn�Aw&�Au��Au%At�As�ArbApZAn��Am%Aj�AiS�Ah�Ah�!Ag�hAe��Ac�^Aa��A_�TA^��A^ �A]�-A\r�AZM�AX�DAV�HAUp�AT��ASAQ��AP��AOl�AMdZAL��AL$�AK7LAJ$�AI"�AH��AG��AFZAD��AC��AAA?7LA=�FA<�+A;��A9�A8 �A7��A6�!A5��A4��A4^5A3�
A3O�A2�/A21'A1��A1oA0ȴA05?A/A.��A.-A-�;A-��A,��A+�FA*v�A)O�A(�/A'�7A&M�A%��A$�RA$1A#t�A"JA!ƨA!�A!?}A!�A r�AƨA�uA�A/A�AE�A�-AO�A�Az�A1A�PA�`A�A��AAz�Ax�A9XAx�AffA�wA�7A�+AS�A-A|�A33A�uA�hA
�HA
ffA	p�A�`A��AA�A�-AC�A��A��A�\AVA�A�A=qA�AdZA�jA��A��A �H@���@��@��`@�r�@� �@�hs@���@�dZ@��@��@�P@�%@�33@�j@�J@�@�ȴ@�b@݁@�Z@�K�@ڟ�@ف@ٺ^@�Z@��T@ԃ@�~�@љ�@϶F@Η�@͙�@�7L@�bN@�@ʧ�@�@Ȭ@ǶF@ư!@ũ�@���@öF@�l�@�E�@��7@�G�@�Z@���@���@��R@��h@��m@�l�@��+@�I�@�+@�$�@�Q�@�M�@��@� �@�K�@��!@�^5@��@��#@�7L@�Ĝ@��@��T@��@�(�@��@��P@���@�@���@��@�A�@���@��@��!@�J@�G�@���@�j@�A�@�1'@�(�@�l�@�ȴ@��@���@��@���@��@�r�@�Q�@� �@�1@��m@��;@��w@�|�@�+@�ȴ@���@�^5@�V@�M�@�E�@�-@�5?@�-@�{@�@���@���@���@���@��@��@��D@�9X@��m@���@�
=@���@�n�@��@�&�@�V@��@���@�bN@�bN@�Z@�Z@�I�@�bN@��@���@��u@���@��@�;d@�"�@�E�@��^@�@��7@���@��h@�hs@�?}@���@��9@�(�@�9X@�z�@�z�@�j@�1@���@�o@�+@��H@�^5@��@�X@��@� �@���@���@�X@��@�dZ@�^5@��@�j@�O�@�n�@���@�-@���@�G�@��@��@�%@��@��@��@�1@��P@�C�@�
=@���@���@�M�@�@��#@���@��^@���@��@���@��u@�Z@�Q�@�Q�@�(�@�1@�  @��m@���@��@��P@�
=@��!@���@�v�@�=q@���@�C�@}�@s"�@kƨ@c�
@[�F@S��@L��@C�
@<��@5�@.@'�@!��@��@��@Z@��@9X@+11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB\)B]/BaHBbNBdZBdZBcTBdZBe`BhsBm�Bu�B�uB�qB�#BB{B2-B@�BA�BA�B>wB<jB9XB1'B,B'�B%�B�BDB��B��B��B��B��B�B�B�B�B�B�B�B�B�B�`B�TB�5B�#B�B��B��B��B��B��BȴBŢB�}B�jB�dB�FB�3B�B�B��B��B��B��B��B��B�{B�JB�%B�B{�Bu�Bm�BiyBffBaHB[#BVBJ�B>wB7LB1'B)�B%�B�BoBPB%B��B�yB�5B��B�dB�!B��B��B�JB~�Bz�Bx�Bs�BjBR�B6FB(�B�B�BB
�B
�#B
��B
��B
�XB
�B
��B
��B
�1B
z�B
o�B
^5B
T�B
O�B
H�B
B�B
;dB
7LB
0!B
+B
&�B
�B
oB
DB
B	��B	�B	�B	�B	�mB	�;B	��B	ɺB	�jB	�9B	�3B	�3B	��B	��B	�=B	� B	y�B	u�B	o�B	dZB	_;B	ZB	T�B	R�B	N�B	J�B	F�B	E�B	C�B	?}B	5?B	.B	&�B	�B	VB	B��B��B�B�B�B�`B�TB�NB�BB�5B�)B�B�
B��B��B��B��B��B��B��B��B��BŢB�wB�XB�9B�'B�B��B��B��B��B��B��B��B��B��B��B��B�{B�uB�oB�hB�bB�VB�PB�JB�DB�=B�7B�+B�B�B�B�B~�B{�Bz�Bx�Bw�Bv�Bt�Br�Bp�Bn�Bm�Bl�BjBiyBgmBffBdZBdZBcTBbNBaHB`BB_;B_;B^5B]/B[#BYBXBXBW
BVBVBS�BR�BQ�BP�BO�BN�BK�BJ�BH�BF�BG�BF�BE�BD�B?}B<jB<jB8RB7LB6FB6FB6FB7LB8RB8RB<jB=qB:^B7LB6FB6FB8RB8RB8RB8RB8RB:^B:^B:^B;dB<jB>wB?}B?}BA�BA�BC�BE�BE�BG�BI�BL�BQ�BN�BL�BL�BL�BQ�BT�BW
B]/B_;BbNBe`BhsBjBk�Bn�Bq�Bw�Bw�Bu�Bw�Bz�B|�B}�B� B�B�B�+B�=B�PB�bB�uB��B��B��B��B��B��B��B��B��B�B�-B�?B�XB�^B�dB�qB�wB�}B��B��B��BBĜBǮBȴBɺB��B��B��B��B��B��B��B��B��B�
B�#B�)B�BB�HB�`B�sB�B�B�B��B��B��B��B	  B	B	B	B	+B	DB	JB	PB	VB	hB	uB	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	"�B	#�B	%�B	'�B	(�B	)�B	/B	33B	8RB	:^B	<jB	<jB	@�B	C�B	G�B	G�B	G�B	F�B	D�B	D�B	G�B	[#B	ffB	jB	l�B	jB	gmB	ffB	ffB	o�B	w�B	y�B	z�B	~�B	�B	�B	�B	�B	�%B	�+B	�1B	�7B	�DB	�PB	�PB	�\B	�\B	�\B	�oB	�uB	�uB	�uB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�-B	ÖB	�5B	�B	��B
%B
\B
�B
#�B
)�B
2-B
:^B
@�B
G�B
O�B
T�B
[#B
_;B
dZB
jB
o�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B^5B_;BcTBcTBdZBdZBcTBdZBe`BhsBm�Bu�B�uB�qB�)B1B�B6FBA�BC�BB�B?}B>wB>wB5?B-B+B+B�BuB%B��B��B��B��B�B�B�B�B��B��B�B�B�B�mB�mB�HB�/B�)B�
B��B��B��B��B��B��B��B�wB�qB�^B�LB�'B�'B�B��B��B��B��B��B��B�\B�7B�B� B{�Bo�Bk�BiyBcTB^5B\)BQ�BA�B9XB49B,B+B�B{BbB	7BB�B�TB��B�}B�?B��B��B�hB� B{�By�Bv�Bq�B^5B;dB,B!�B�BPB
�B
�5B
��B
ÖB
�wB
�'B
��B
��B
�JB
~�B
v�B
bNB
W
B
R�B
J�B
F�B
>wB
:^B
33B
,B
+B
!�B
�B
bB
+B	��B	�B	�B	�B	�B	�ZB	�B	��B	��B	�?B	�?B	�LB	�B	��B	�\B	�B	{�B	y�B	v�B	gmB	dZB	`BB	VB	VB	Q�B	N�B	I�B	G�B	E�B	C�B	9XB	0!B	-B	�B	hB	1B	B��B�B�B�B�mB�`B�ZB�NB�BB�/B�#B�B�
B��B��B��B��B��B��B��B��BɺBB�qB�FB�?B�!B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�uB�oB�bB�\B�PB�PB�JB�DB�7B�%B�%B�B�B�B~�B|�B{�By�Bw�Bw�Bu�Bs�Bp�Bn�Bn�Bm�Bk�BiyBiyBffBe`BdZBdZBcTBaHB`BB`BB_;B^5B^5B\)BZBYBYBXBW
BW
BVBR�BS�BP�BO�BO�BL�BJ�BK�BJ�BJ�BI�BG�BC�B@�B?}B;dB;dB:^B8RB8RB7LB:^B8RB?}BA�B=qB:^B7LB8RB9XB9XB9XB9XB:^B;dB;dB<jB<jB>wB@�B@�BA�BB�BC�BD�BF�BG�BH�BJ�BL�BS�BQ�BM�BN�BO�BS�BW
BZB`BBaHBcTBffBiyBk�Bl�Bo�Br�Bx�By�Bx�By�B{�B|�B~�B�B�B�%B�1B�DB�VB�hB�{B��B��B��B��B��B��B��B��B�B�B�3B�FB�XB�dB�dB�qB�wB�}B��B��B��BÖBŢBȴBȴBɺB��B��B��B��B��B��B��B��B��B�
B�#B�)B�BB�NB�fB�yB�B�B�B��B��B��B��B	  B	B	B	%B	+B	DB	JB	PB	VB	hB	uB	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	"�B	#�B	%�B	(�B	)�B	+B	/B	33B	8RB	:^B	=qB	=qB	A�B	C�B	H�B	G�B	H�B	G�B	F�B	D�B	C�B	XB	ffB	k�B	n�B	k�B	hsB	hsB	e`B	m�B	w�B	z�B	{�B	� B	�B	�B	�B	�B	�%B	�+B	�7B	�=B	�JB	�PB	�VB	�\B	�bB	�bB	�oB	�uB	�uB	�uB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�-B	ÖB	�5B	�B	��B
%B
\B
�B
$�B
)�B
2-B
:^B
@�B
G�B
O�B
T�B
[#B
_;B
dZB
jB
o�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0 dbar.                                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447052012010314470520120103144705  AO  ARGQ                                                                        20111130141212  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130141212  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144705  IP                  G�O�G�O�G�O�                