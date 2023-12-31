CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:21Z UW 3.1 conversion   
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
_FillValue                    �hArgo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               mA   AO  20111130141636  20190522121826  1727_5046_109                   2C  D   APEX                            2143                            040306                          846 @Ԣ���?�1   @Ԣ�b�` @6[��S���c�-V1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @333@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C/�fC2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CS�fCU�fCX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW�fDX  DX� DY  DY� DZ  DZy�D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da�fDb  Db� Dc  Dc� Dd  Dd�fDe  De� Df  Df� Dg  Dg� Dh  Dhy�Di  Di� Dj  Dj� Dk  Dk� Dk��Dl� DmfDm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr�fDs  Dy�31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @333@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C/�fC2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CS�fCU�fCX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW�fDX  DX� DY  DY� DZ  DZy�D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da�fDb  Db� Dc  Dc� Dd  Dd�fDe  De� Df  Df� Dg  Dg� Dh  Dhy�Di  Di� Dj  Dj� Dk  Dk� Dk��Dl� DmfDm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr�fDs  Dy�31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�$�A��A�A���A�v�A�?}A�%AͲ-Aͺ^Aͺ^Aʹ9Aͣ�A͋DA͉7A�K�A�"�A�ĜA�A�A˰!A� �A��HA��A���Aȕ�A�1'A���A���A��FA�hsA�Q�A�33A���A��
A�n�A���A�"�A���A�l�A�+A���A�~�A���A�^5A�7LA��9A���A�O�A�33A���A��uA�C�A���A�XA�;dA�^5A���A��!A�S�A�&�A���A�S�A��HA��7A�C�A��A�  A���A�`BA�A��!A�-A��jA���A�hsA��HA��HA�VA���A�I�A�A��\A�ĜA��#A���A�ĜA�VA��RA� �A���A�M�A�7LA�%A�VA�Q�A��\A��wA��A�dZA�
=A���A���A�O�A��/A���A�C�A�1'A���A�K�A��!A���A�=qA��AzQ�Avr�Au�mAu�PAt��Asl�Ao\)Aj9XAh�9AgoAd�A`�A]��A\9XAZ�DAW"�AV��AVA�AUVAT�9AT �AR�HAR=qAQ��AP-AO7LAN��ANQ�AK��AI�mAI��AHM�AF1ADZAChsAB��AA��AA7LA@�A?��A>�A>�!A>z�A=�;A<�/A;\)A9�FA8�9A7�-A6��A6^5A69XA6bA5��A4��A3�;A0��A/�
A/33A.��A,~�A+O�A)�7A(�RA(~�A(Q�A'�FA&-A%33A$ZA#�A"$�A!�TA!��A!33A ĜA�AjA33A�!An�A  A�7A��A�A��A^5A��A\)A�A�+A��AO�A�mAAjAĜA�^AA^5A��A�-A
��A	hsA�DA�A��A&�A�An�AI�A��A�hA��A{A �@���@��u@��-@�\)@���@�z�@�ƨ@�P@�l�@�
=@�ȴ@�~�@�@�A�@�h@�bN@��
@땁@�\)@�+@�o@�-@�O�@�ƨ@��@�D@ܓu@ڸR@�@�Q�@ׅ@�n�@���@���@�
=@ҟ�@�X@��m@�+@͡�@�`B@�%@˥�@�M�@���@�z�@�A�@��@�ƨ@ǝ�@�t�@�S�@Ɨ�@�bN@�
=@¸R@��@���@�`B@�/@���@�r�@�bN@�1@�;d@���@�^5@��^@�r�@�|�@�J@��m@�/@�1@�C�@�ȴ@���@�$�@���@��T@���@�E�@���@��@���@��u@�1@�S�@�?}@�S�@�E�@���@�G�@�V@�Ĝ@�z�@�Q�@�(�@�I�@�j@��u@�Ĝ@��h@��@�=q@�{@�hs@�G�@�X@�?}@�G�@�X@��h@��@�"�@���@��#@���@���@�%@��@��@�=q@��@�p�@�O�@�x�@�`B@��@���@���@�M�@�
=@�C�@��@��H@���@���@�~�@�n�@�V@�@�G�@��D@�A�@�~�@�r�@�z�@��9@��@���@�j@�Z@�I�@�A�@��@���@��;@���@�K�@�;d@�"�@��y@��@��@��@�^5@���@��-@���@�p�@�O�@�?}@�&�@��@�V@�%@��@��@�I�@�A�@� �@��@�ƨ@�K�@�^5@��@�{@��@��@��j@�1@�o@�ȴ@��\@�M�@�=q@�=q@��@�J@��#@��^@��-@��-@���@��7@�hs@�/@�bN@�1'@���@�"�@�ȴ@�=q@�@�p�@�&�@���@��`@�Ĝ@��j@��9@��@���@��D@�z�@�A�@� �@�1@�1@�1@�b@� �@��@�1@��m@��P@�K�@�"�@�
=@��@��!@��!@��!@���@���@��!@���@�^5@���@�%@�z�@��m@��@��@�o@��@���@l�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�$�A��A�A���A�v�A�?}A�%AͲ-Aͺ^Aͺ^Aʹ9Aͣ�A͋DA͉7A�K�A�"�A�ĜA�A�A˰!A� �A��HA��A���Aȕ�A�1'A���A���A��FA�hsA�Q�A�33A���A��
A�n�A���A�"�A���A�l�A�+A���A�~�A���A�^5A�7LA��9A���A�O�A�33A���A��uA�C�A���A�XA�;dA�^5A���A��!A�S�A�&�A���A�S�A��HA��7A�C�A��A�  A���A�`BA�A��!A�-A��jA���A�hsA��HA��HA�VA���A�I�A�A��\A�ĜA��#A���A�ĜA�VA��RA� �A���A�M�A�7LA�%A�VA�Q�A��\A��wA��A�dZA�
=A���A���A�O�A��/A���A�C�A�1'A���A�K�A��!A���A�=qA��AzQ�Avr�Au�mAu�PAt��Asl�Ao\)Aj9XAh�9AgoAd�A`�A]��A\9XAZ�DAW"�AV��AVA�AUVAT�9AT �AR�HAR=qAQ��AP-AO7LAN��ANQ�AK��AI�mAI��AHM�AF1ADZAChsAB��AA��AA7LA@�A?��A>�A>�!A>z�A=�;A<�/A;\)A9�FA8�9A7�-A6��A6^5A69XA6bA5��A4��A3�;A0��A/�
A/33A.��A,~�A+O�A)�7A(�RA(~�A(Q�A'�FA&-A%33A$ZA#�A"$�A!�TA!��A!33A ĜA�AjA33A�!An�A  A�7A��A�A��A^5A��A\)A�A�+A��AO�A�mAAjAĜA�^AA^5A��A�-A
��A	hsA�DA�A��A&�A�An�AI�A��A�hA��A{A �@���@��u@��-@�\)@���@�z�@�ƨ@�P@�l�@�
=@�ȴ@�~�@�@�A�@�h@�bN@��
@땁@�\)@�+@�o@�-@�O�@�ƨ@��@�D@ܓu@ڸR@�@�Q�@ׅ@�n�@���@���@�
=@ҟ�@�X@��m@�+@͡�@�`B@�%@˥�@�M�@���@�z�@�A�@��@�ƨ@ǝ�@�t�@�S�@Ɨ�@�bN@�
=@¸R@��@���@�`B@�/@���@�r�@�bN@�1@�;d@���@�^5@��^@�r�@�|�@�J@��m@�/@�1@�C�@�ȴ@���@�$�@���@��T@���@�E�@���@��@���@��u@�1@�S�@�?}@�S�@�E�@���@�G�@�V@�Ĝ@�z�@�Q�@�(�@�I�@�j@��u@�Ĝ@��h@��@�=q@�{@�hs@�G�@�X@�?}@�G�@�X@��h@��@�"�@���@��#@���@���@�%@��@��@�=q@��@�p�@�O�@�x�@�`B@��@���@���@�M�@�
=@�C�@��@��H@���@���@�~�@�n�@�V@�@�G�@��D@�A�@�~�@�r�@�z�@��9@��@���@�j@�Z@�I�@�A�@��@���@��;@���@�K�@�;d@�"�@��y@��@��@��@�^5@���@��-@���@�p�@�O�@�?}@�&�@��@�V@�%@��@��@�I�@�A�@� �@��@�ƨ@�K�@�^5@��@�{@��@��@��j@�1@�o@�ȴ@��\@�M�@�=q@�=q@��@�J@��#@��^@��-@��-@���@��7@�hs@�/@�bN@�1'@���@�"�@�ȴ@�=q@�@�p�@�&�@���@��`@�Ĝ@��j@��9@��@���@��D@�z�@�A�@� �@�1@�1@�1@�b@� �@��@�1@��m@��P@�K�@�"�@�
=@��@��!@��!@��!@���@���@��!@���@�^5@���@�%@�z�@��m@��@��@�o@��@���@l�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B�oB�%B�=B�7B�Bz�B~�B� B�B�%B�7B�7B�uB��B��B�\B�+B�PB�{B�{B��B��B��B��B��B��B��B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B��B��B��B��B��B��B��B��B�JB�=B�=B�+B�%B�B�B}�By�Bu�Bs�Br�Br�Bw�B�%B�B}�B~�Bp�BiyBgmB_;BD�B/B�B�B{B
=B��B�B�yB�NB�)B��B�dB��B��B�oB�PB� BgmB-B�B�B�BbB+B
��B
�B
�#B
B
�3B
��B
p�B
L�B
49B
"�B
oB	��B	ȴB	�B	��B	��B	��B	�VB	m�B	P�B	E�B	9XB	(�B	�B	B��B��B�B�B�B��B	B	+B	�B	)�B	+B	+B	&�B	"�B	�B	�B	VB	
=B	
=B	%B	  B��B��B��B�B�B�B�B�B�yB�mB�NB�)B�
B��B��B��B��B��B��BǮB��B�9B��B��B��B�{B�B�B�B�B�B�B� B�B�B|�Bu�Bp�Bo�Bm�Bl�BjBiyBhsBiyBiyBhsBhsBgmBdZBcTBcTBcTBbNBbNBaHB`BB`BB]/BZBZB\)B]/B]/B\)B[#B[#BYBW
BVBT�BR�BQ�BN�BL�BM�BM�BK�BJ�BG�BB�B=qB>wBB�BB�B>wB:^B9XB;dB<jB=qB@�BA�B@�B@�BA�BE�BF�BE�BE�BE�BE�BE�BH�BJ�BJ�BJ�BG�BN�BO�BO�BP�BR�BT�BXBZB\)B\)B^5BcTBcTBcTBdZBcTBcTBffBhsBhsBhsBhsBhsBhsBhsBhsBjBp�Bn�Bo�Bp�Bq�Bq�Bq�Bs�Bs�Bs�Bt�B{�B�B�B�7B�PB�VB�PB�1B�=B�JB�VB�\B�\B�\B�uB��B�!B�FB�RB�XB�XB�RB�RB�^BB��B��B�#B�BB�TB�sB�B�B�B��B��B��B��B	+B	oB	�B	�B	 �B	!�B	%�B	(�B	)�B	(�B	)�B	.B	1'B	1'B	2-B	2-B	8RB	6FB	2-B	?}B	S�B	W
B	XB	YB	ZB	[#B	[#B	VB	T�B	YB	`BB	cTB	dZB	dZB	e`B	ffB	gmB	hsB	hsB	k�B	k�B	jB	k�B	hsB	p�B	x�B	|�B	|�B	|�B	}�B	�B	�B	�B	�=B	�PB	�VB	�bB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�-B	�3B	�3B	�?B	�LB	�jB	�wB	��B	B	B	ÖB	ÖB	B	B	B	B	��B	B	B	B	B	ÖB	B	ÖB	ÖB	ŢB	ƨB	ǮB	ȴB	ȴB	ɺB	ɺB	ɺB	ɺB	ɺB	ɺB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	�B	�
B	�B	�B1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��B��B�1B�DB�=B�Bz�B~�B� B�B�%B�7B�=B�{B��B��B�oB�7B�VB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�!B��B��B��B��B��B��B��B��B�hB�JB�JB�7B�+B�+B�B�B{�Bw�Bt�Bs�Br�Bz�B�1B�B�B�+Bt�Bl�BiyBgmBL�B5?B�B�B�BVB��B�B�B�`B�5B�)BǮB��B��B�uB�hB�%Bz�B1'B�B�B�BuBDB
��B
��B
�HB
ǮB
�^B
��B
{�B
W
B
9XB
'�B
�B
	7B	��B	�!B	��B	��B	��B	��B	x�B	S�B	H�B	?}B	/B	�B	1B	B	B��B��B��B	  B	B	DB	�B	-B	0!B	/B	'�B	&�B	(�B	�B	bB	bB	uB	PB	B	  B��B��B��B��B�B�B�B�B�B�sB�NB�#B�
B��B��B��B��B��BɺBB�^B��B��B��B��B�1B�%B�B�B�B�B�B�%B�B�Bx�Bq�Bp�Bo�Bn�Bn�Bn�Bm�Bk�BjBjBjBjBhsBgmBffBe`BdZBcTBdZBcTBcTBcTBbNB_;BbNBaHB`BB^5B]/B\)B\)B\)BYBW
BS�BT�BVBN�BN�BN�BM�BL�BJ�BF�B?}BA�BF�BE�B@�B<jB:^B<jB<jB>wBA�BB�BA�BC�BE�BG�BG�BF�BF�BE�BE�BG�BJ�BM�BM�BM�BS�BQ�BQ�BQ�BQ�BT�BW
BZB[#B]/B^5B`BBdZBe`BcTBe`Be`Be`BhsBiyBhsBiyBhsBhsBhsBhsBiyBm�Br�Bo�Bp�Bq�Br�Bq�Br�Bs�Bs�Bt�Bu�B|�B�B�%B�DB�\B�bB�bB�JB�JB�PB�\B�\B�bB�oB��B��B�'B�LB�XB�^B�^B�XB�XB�wBŢB��B�B�)B�HB�ZB�yB�B�B�B��B��B��B��B	%B	oB	�B	�B	 �B	!�B	%�B	(�B	)�B	(�B	+B	2-B	49B	1'B	33B	2-B	:^B	;dB	0!B	<jB	S�B	XB	XB	YB	ZB	\)B	^5B	W
B	S�B	XB	`BB	cTB	dZB	dZB	e`B	ffB	gmB	hsB	iyB	l�B	l�B	k�B	n�B	k�B	p�B	x�B	|�B	|�B	|�B	}�B	�B	�B	�B	�=B	�PB	�\B	�hB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�-B	�3B	�3B	�FB	�RB	�qB	�}B	��B	B	B	ÖB	ÖB	B	B	B	B	��B	B	B	B	B	ĜB	ÖB	ÖB	ĜB	ƨB	ǮB	ȴB	ɺB	ɺB	��B	ɺB	ɺB	ɺB	ɺB	ɺB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	��B	��B	��B	�
B	�
B	�B	�B1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<D��<#�
<#�
<#�
<#�
<#�
<���<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<e`B<49X<#�
<#�
<#�
<49X<���<D��<#�
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
<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0 dbar.                                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447122012010314471220120103144712  AO  ARGQ                                                                        20111130141636  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130141636  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144712  IP                  G�O�G�O�G�O�                