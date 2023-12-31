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
_FillValue                    �hArgo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               YA   AO  20111130141158  20190522121826  1727_5046_089                   2C  D   APEX                            2143                            040306                          846 @Ԉ��п�1   @Ԉ�@@7���"���c�I�^51   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @@  @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BXffB`ffBh  Bp  Bw��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP�CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D fD � D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&�fD'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`�fDafDa� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� DpfDp� Dq  Dq� Dr  Dr� Ds  Ds� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @@  @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BXffB`ffBh  Bp  Bw��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP�CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D fD � D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&�fD'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`�fDafDa� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� DpfDp� Dq  Dq� Dr  Dr� Ds  Ds� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�dZA�dZA�n�A�l�A�l�A�r�A�t�A�p�A�n�A�jA�n�A�r�A�p�A�v�A�r�A�r�A�r�A�t�A�t�A�t�A�v�A�x�A�x�A�v�A�x�A�|�A�|�A�^5A�9XA�1'A�$�A�?}A��Aɩ�A�  A��;A�A�A�AŅAāA��TA��\A�-A��-A�|�A�
=A�{A�dZA�VA���A�`BA�?}A���A�ƨA�G�A��A�{A��A�bA�A��A��jA�p�A���A�dZA���A��^A� �A���A���A��`A�r�A��
A���A�ffA��FA�r�A��TA�ffA�VA�x�A�VA�~�A�r�A���A���A���A��TA��A���A���A�ffA�z�A���A�7LA�t�A�oA�ƨA�Q�A�oA�O�A��TA�1A�;dA���A�33A��!A�ĜA�+A���A�(�A��yA���A��A�E�A���A�K�A��7A��A��TA��A�C�A�`BA�p�A��A�^5A��A~9XA|�9A{�wAy��AyVAw�
AvM�Au/Ast�ArȴAq|�Ap�Am��Ak�#Aj�9AiXAh �Ae+Aa`BA`M�A^��A^  A\�A\~�A[�#AZ�9AY"�AWx�AU��AT�\AT=qAS|�ARv�AQG�APVAO�ANn�AMAL��AK`BAI�FAI�^AHA�AFE�AE&�ACl�AB~�AA�TAA+A@Q�A?;dA>�A=��A;�A:�A:9XA9��A9S�A7�mA6�9A61'A5"�A4�A3�PA3oA2ȴA2(�A1C�A0(�A/��A.�9A-hsA,��A, �A*��A)�
A)7LA)�A(ffA'`BA&��A&(�A%��A$�`A$ZA#A#x�A"��A"I�A!�A jA 1A�PAE�A
=A��A�;A"�A$�A��A  A�PAdZA�`At�AffA�
A��A?}A��A�TA33Ar�A�yA��A?}A
bNA	hsA��A&�Az�A  A��AAO�A��AI�A\)A �+@��m@�n�@�`B@���@�K�@���@�l�@�ff@��7@���@���@��@���@�{@���@���@�7@�Z@�{@�Z@�\)@��@�!@�K�@�|�@�@��u@�Z@߶F@ݙ�@ܼj@�33@���@�`B@ش9@���@�-@ԓu@ӕ�@���@ҏ\@�{@��@�O�@�ȴ@���@�7L@�33@�hs@ȴ9@���@ț�@�1'@�j@��/@�%@���@��@�M�@�x�@��@��R@�9X@��@�C�@�@���@�v�@�E�@��#@��@���@�E�@��D@�t�@��@�$�@�J@��7@��F@���@�{@�@���@�7L@���@�9X@��m@�;d@���@��@���@���@�7L@��u@��;@���@��
@��@���@��+@�33@���@�j@�1'@��@�Ĝ@�O�@�Ĝ@�^5@�K�@��j@�
=@�X@�j@�Q�@���@��@�dZ@�ff@�E�@�-@�5?@��#@�=q@�o@�ȴ@���@��+@�X@���@�@��#@�@��@�{@��#@�`B@�V@�Ĝ@��j@��@�z�@�Q�@��@�ȴ@��#@�hs@�%@�  @�K�@��@��\@�V@�$�@�hs@���@��@�=q@�`B@���@�9X@���@�"�@��R@�^5@�-@��@��h@�X@��@��D@�9X@�9X@�(�@��@��@��m@��
@��w@��;@��@��@� �@��
@�(�@��@��u@�9X@�1@��
@�  @�K�@��@�
=@���@��@�K�@�\)@�l�@�t�@��y@�\)@�+@�o@�o@�
=@�ȴ@�~�@�v�@�ff@�-@��T@�@��^@���@�7L@��@���@���@���@�bN@�A�@�1'@��m@��w@���@��#@���@��@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�dZA�dZA�n�A�l�A�l�A�r�A�t�A�p�A�n�A�jA�n�A�r�A�p�A�v�A�r�A�r�A�r�A�t�A�t�A�t�A�v�A�x�A�x�A�v�A�x�A�|�A�|�A�^5A�9XA�1'A�$�A�?}A��Aɩ�A�  A��;A�A�A�AŅAāA��TA��\A�-A��-A�|�A�
=A�{A�dZA�VA���A�`BA�?}A���A�ƨA�G�A��A�{A��A�bA�A��A��jA�p�A���A�dZA���A��^A� �A���A���A��`A�r�A��
A���A�ffA��FA�r�A��TA�ffA�VA�x�A�VA�~�A�r�A���A���A���A��TA��A���A���A�ffA�z�A���A�7LA�t�A�oA�ƨA�Q�A�oA�O�A��TA�1A�;dA���A�33A��!A�ĜA�+A���A�(�A��yA���A��A�E�A���A�K�A��7A��A��TA��A�C�A�`BA�p�A��A�^5A��A~9XA|�9A{�wAy��AyVAw�
AvM�Au/Ast�ArȴAq|�Ap�Am��Ak�#Aj�9AiXAh �Ae+Aa`BA`M�A^��A^  A\�A\~�A[�#AZ�9AY"�AWx�AU��AT�\AT=qAS|�ARv�AQG�APVAO�ANn�AMAL��AK`BAI�FAI�^AHA�AFE�AE&�ACl�AB~�AA�TAA+A@Q�A?;dA>�A=��A;�A:�A:9XA9��A9S�A7�mA6�9A61'A5"�A4�A3�PA3oA2ȴA2(�A1C�A0(�A/��A.�9A-hsA,��A, �A*��A)�
A)7LA)�A(ffA'`BA&��A&(�A%��A$�`A$ZA#A#x�A"��A"I�A!�A jA 1A�PAE�A
=A��A�;A"�A$�A��A  A�PAdZA�`At�AffA�
A��A?}A��A�TA33Ar�A�yA��A?}A
bNA	hsA��A&�Az�A  A��AAO�A��AI�A\)A �+@��m@�n�@�`B@���@�K�@���@�l�@�ff@��7@���@���@��@���@�{@���@���@�7@�Z@�{@�Z@�\)@��@�!@�K�@�|�@�@��u@�Z@߶F@ݙ�@ܼj@�33@���@�`B@ش9@���@�-@ԓu@ӕ�@���@ҏ\@�{@��@�O�@�ȴ@���@�7L@�33@�hs@ȴ9@���@ț�@�1'@�j@��/@�%@���@��@�M�@�x�@��@��R@�9X@��@�C�@�@���@�v�@�E�@��#@��@���@�E�@��D@�t�@��@�$�@�J@��7@��F@���@�{@�@���@�7L@���@�9X@��m@�;d@���@��@���@���@�7L@��u@��;@���@��
@��@���@��+@�33@���@�j@�1'@��@�Ĝ@�O�@�Ĝ@�^5@�K�@��j@�
=@�X@�j@�Q�@���@��@�dZ@�ff@�E�@�-@�5?@��#@�=q@�o@�ȴ@���@��+@�X@���@�@��#@�@��@�{@��#@�`B@�V@�Ĝ@��j@��@�z�@�Q�@��@�ȴ@��#@�hs@�%@�  @�K�@��@��\@�V@�$�@�hs@���@��@�=q@�`B@���@�9X@���@�"�@��R@�^5@�-@��@��h@�X@��@��D@�9X@�9X@�(�@��@��@��m@��
@��w@��;@��@��@� �@��
@�(�@��@��u@�9X@�1@��
@�  @�K�@��@�
=@���@��@�K�@�\)@�l�@�t�@��y@�\)@�+@�o@�o@�
=@�ȴ@�~�@�v�@�ff@�-@��T@�@��^@���@�7L@��@���@���@���@�bN@�A�@�1'@��m@��w@���@��#@���@��@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB/B.B-B.B.B-B-B.B.B/B.B.B.B.B.B.B.B.B.B/B/B.B.B.B.B.B.B,B&�B%�B'�B,B(�B&�B&�B-B6FB<jB;dB7LB/B&�B=qBH�BJ�BP�BdZBw�B�bB��B�3B�9B�RBƨB��BƨB��BŢBǮBǮBɺBȴBɺB��BĜB�wB�jB�jB�dB�^B�RB�?B�9B�3B�'B�B�B��B��B��B�{B�PB�7B~�Bw�Br�BjBaHBVBD�B@�B:^B1'B(�B#�B�B{B%B  B��B�B�`B�B��BĜB�^B��B��B�7Bl�BJ�B49B"�BoBB
��B
�B
��B
�wB
��B
�JB
}�B
bNB
Q�B
I�B
C�B
<jB
5?B
-B
,B
49B
/B
'�B
�B
�B
�B
�B
uB
DB
B	��B	�B	�B	�BB	��B	�wB	�XB	�3B	�!B	�B	��B	��B	��B	��B	�uB	�7B	�B	�B	|�B	v�B	w�B	k�B	e`B	bNB	^5B	YB	R�B	O�B	S�B	I�B	=qB	8RB	/B	+B	+B	&�B	!�B	�B	�B	�B	JB	1B	%B	B	B��B��B�B�B�B�sB�fB�ZB�HB�)B�
B��B��B��BȴBŢBÖB��B�wB�jB�^B�FB�9B�-B�!B�B�B�B��B��B��B��B��B��B��B��B��B�uB�bB�VB�DB�1B�B�B�B�B�B|�Bz�Bv�Bu�Bs�Bp�Bo�Bk�BjBhsBe`BcTBaHB^5B[#BYBW
BS�BQ�BP�BO�BL�BK�BJ�BI�BH�BG�BF�BD�BC�BB�BB�BB�BB�BA�B@�B>wB=qB<jB;dB:^B8RB6FB49B33B33B9XB@�B@�B6FB33B6FB5?B33B33B6FB5?B7LB6FB5?B33B0!B2-B49B49B6FB<jB>wB@�BB�B@�B?}B?}BA�BG�BJ�BQ�BYB\)B[#BT�BW
BXB\)B_;B_;B_;BaHBbNBbNBcTBe`BffBhsBhsBe`Be`BffBe`Be`BhsBl�Bl�Bk�Bn�Bp�Br�Br�Bs�Bs�Bu�Bv�Bw�Bx�B}�B� B�B�+B�=B�\B�oB��B��B��B��B�!B�qBĜBɺB��B�B�/B�/B�
B��BǮB��B��BȴB��B��B��B�
B�#B�)B�/B�5B�BB�mB�B�B�B�B�B��B	  B	B	B	B	+B		7B		7B	JB	oB	{B	�B	�B	{B	uB	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	.B	1'B	33B	2-B	1'B	2-B	33B	33B	49B	6FB	8RB	9XB	:^B	=qB	?}B	B�B	D�B	F�B	H�B	I�B	K�B	O�B	P�B	T�B	W
B	YB	]/B	_;B	`BB	dZB	gmB	iyB	iyB	jB	k�B	l�B	k�B	k�B	l�B	m�B	p�B	t�B	w�B	y�B	{�B	~�B	�B	�%B	�+B	�1B	�7B	�7B	�1B	�1B	�7B	�JB	�hB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B/B.B-B.B.B-B-B.B.B/B.B.B.B.B.B.B.B.B.B/B/B.B.B.B.B.B.B.B(�B'�B/B/B)�B(�B+B0!B9XB?}B?}B>wB5?B.B?}BI�BM�BVBhsBy�B�oB��B�LB�LB�dBȴB��BɺBÖBǮBȴB��B��B��B��B��BƨB�}B�}B�wB�qB�wB�dB�RB�?B�?B�?B�9B�B��B��B��B��B�hB�PB�%By�Bv�Bo�BhsB_;BG�BC�B?}B6FB,B'�B�B�B	7BB  B�B�B�/B��BƨB��B�B��B�bBv�BO�B9XB&�B�B+B  B
��B
�#B
ĜB
�!B
�bB
�+B
gmB
T�B
L�B
F�B
@�B
9XB
0!B
2-B
6FB
33B
-B
#�B
"�B
�B
�B
�B
oB

=B	��B	��B	�B	�B	�/B	B	�wB	�?B	�3B	�B	�B	�B	��B	��B	��B	�JB	�B	�B	� B	y�B	z�B	n�B	gmB	dZB	aHB	]/B	XB	O�B	XB	O�B	@�B	=qB	2-B	-B	-B	)�B	$�B	�B	�B	�B	\B	
=B	+B	1B	1B	  B��B��B�B�B�B�mB�fB�ZB�BB�B�B��B��B��B��BǮBB�}B�}B�qB�RB�?B�3B�-B�'B�B�B�B��B��B��B��B��B��B��B��B��B�oB�hB�VB�DB�%B�B�B�B�B~�B}�B{�Bv�Bw�Br�Br�Bp�Bm�Bk�BhsBffBdZBcTB^5B[#B[#BXBS�BR�BR�BP�BN�BJ�BL�BJ�BH�BI�BD�BF�BD�BD�BC�BC�BB�BB�BB�B@�B@�B>wB=qB<jB:^B6FB49B33B8RBA�BH�B6FB49B7LB8RB49B5?B8RB6FB8RB9XB6FB5?B2-B33B5?B5?B6FB=qBB�BB�BC�BC�BB�B@�BA�BG�BK�BQ�BYB\)BcTBW
BXBZB]/BcTBcTB`BBbNBcTBcTBdZBffBgmBiyBl�BhsBhsBhsBffBffBhsBm�Bp�Bm�Bo�Bq�Br�Bs�Bt�Bt�Bv�Bw�Bx�By�B}�B� B�B�1B�DB�\B�oB��B��B��B��B�B�jBĜBɺB��B�B�5B�HB�)B��B��BĜBÖBȴB��B��B�B�B�#B�)B�/B�;B�;B�fB�B�B�B�B�B��B	  B	B	B	B	+B	
=B	
=B	JB	oB	{B	�B	�B	�B	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	.B	2-B	5?B	33B	2-B	33B	49B	49B	5?B	6FB	8RB	:^B	;dB	>wB	@�B	C�B	D�B	F�B	H�B	I�B	K�B	O�B	P�B	T�B	W
B	YB	]/B	`BB	_;B	cTB	gmB	jB	iyB	jB	k�B	m�B	k�B	k�B	l�B	m�B	o�B	t�B	w�B	y�B	|�B	}�B	�B	�%B	�+B	�1B	�=B	�=B	�1B	�1B	�=B	�PB	�hB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0 dbar.                                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447052012010314470520120103144705  AO  ARGQ                                                                        20111130141158  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130141158  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144705  IP                  G�O�G�O�G�O�                