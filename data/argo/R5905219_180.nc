CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2023-03-04T15:47:46Z creation;2023-03-04T15:47:55Z conversion to V3.1      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8<   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8@   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8H   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8L   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8T   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8\   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8d   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8h   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8p   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8t   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8x   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8|   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9|   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `h   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �l   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �P   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �T   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �8   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �X   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �D   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �H   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �L   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �P   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �T   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20230304154746  20230304155812  5905219                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            7906                            051216                          846 @�X��5�1   @�Y��>�@5Q&�x���c�bM��1   GPS     A   A   F   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @333@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`ffBh  Bp  Bx  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�33B�  B���B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C1�fC3�fC6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL�CN  CO�fCR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz�C|  C}�fC�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��C��C��C��C�  C�  C��3C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C��C�  D   D � D  D� D��Dy�D  D� D  D� D  D�fD  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#fD#�fD$fD$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.fD.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4fD4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D:��D;� D<fD<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DA��DB� DC  DC� DD  DD�fDE  DE� DFfDF�fDGfDG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM�fDN  DN� DOfDO� DP  DP� DQ  DQy�DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`�fDa  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df�fDg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dk��Dl� Dm  Dm� DnfDn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� DxfDx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D��3D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D���D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D���D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D��3D�  D�@ D�� D�� D�  D�<�D�|�D�� D�  D�@ D��3D�� D�  D�<�D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D��3D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�<�Dɀ D�� D�  D�@ Dʀ D��3D�3D�@ Dˀ D�� D�  D�@ D̃3D��3D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�<�DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�C3DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�C3D؃3D��3D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��D�  D�@ D� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@N{@�p�@�p�A�RA&�RAF�RAf�RA�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)B�B	�B�B�B!�B)�B1�B9�BA�BI�BQ�BY�BbzBi�Bq�By�B��
B��
B��
B��
B��
B�
=B�
=B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
Bأ�B��
B��
B��
B��
B�
=B��
B���B��
B��
C k�Ck�Ck�Ck�Ck�C
k�Ck�Ck�Ck�Ck�Ck�Ck�Ck�Ck�Ck�Ck�C k�C"k�C$k�C&k�C(k�C*k�C,k�C.k�C0k�C2Q�C4Q�C6k�C8k�C:k�C<k�C>k�C@k�CBk�CDk�CFk�CHk�CJk�CL�CNk�CPQ�CRk�CTk�CVk�CXk�CZk�C\k�C^k�C`k�Cbk�Cdk�Cfk�Chk�Cjk�Clk�Cnk�Cpk�Crk�Ctk�Cvk�Cxk�Cz�C|k�C~Q�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�(�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�(�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�(�C�5�C�5�C�5�C�5�C�5�C�B�C�B�C�B�C�B�C�5�C�5�C�(�C�5�C�5�C�(�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�B�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�(�C�5�C�5�C�5�C�5�C�5�C�(�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�B�C�B�C�5�C�5�C�5�C�5�C�B�C�5�D �D ��D�D��D{D�{D�D��D�D��D�D�GD�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D!GD�GD�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#!GD#�GD$!GD$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.!GD.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4!GD4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;{D;��D<!GD<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB{DB��DC�DC��DD�DD�GDE�DE��DF!GDF�GDG!GDG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM�GDN�DN��DO!GDO��DP�DP��DQ�DQ�{DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`�GDa�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df�GDg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl{Dl��Dm�Dm��Dn!GDn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx!GDx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D��D�qD�MqD���D��qD�qD�MqD��qD��>D�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��>D��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD���D��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD�ФD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�
>D�MqD��qD��>D�
>D�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��>D��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��>D�
>D�MqD��qD��qD�qD�MqD��qD��qD�
>D�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�P�D��qD�ФD�qD�MqD��qD��qD�qD�J>D��>D��qD�qD�MqD���D��qD�qD�J>D��qD��qD�qD�MqDqD��qD�qD�MqDÍqD��qD�qD�MqDčqD��qD�qD�MqDōqD��qD�qD�MqDƍqD�ФD�qD�MqDǍqD��qD�qD�MqDȍqD��qD�qD�J>DɍqD��qD�qD�MqDʍqD�ФD��D�MqDˍqD��qD�qD�MqD̐�D�ФD�qD�MqD͍qD��qD�qD�MqD΍qD��qD�qD�MqDύqD��qD�qD�J>DЍqD��qD�qD�MqDэqD��qD�qD�MqDҍqD��qD�qD�MqDӍqD��qD�qD�P�DԍqD��qD�qD�MqDՍqD��qD�qD�MqD֍qD��qD�qD�MqD׍qD��qD�qD�P�Dؐ�D�ФD�qD�MqDٍqD��qD�qD�MqDڍqD��qD�qD�MqDۍqD��qD�qD�MqD܍qD��qD�qD�MqDݍqD��qD�qD�MqDލqD��qD�qD�MqDߍqD��qD�qD�MqD��qD��qD�qD�MqD�qD��qD�qD�MqD�qD��qD��D�MqD�qD��qD�qD�MqD�qD��qD�qD�MqD�qD��qD�qD�MqD�qD��qD�qD�MqD�qD��qD�qD�MqD�qD��qD�qD�MqD�qD��qD�qD�P�D�qD��qD�qD�MqD�qD��qD�qD�MqD�qD��qD�qD�MqD�qD��qD�qD�MqD�qD��qD�qD�MqD�qD��qD�qD�P�D��qD��qD�qD�MqD�qD��qD�qD�MqD�qD��qD�qD�MqD�qD��>D�qD�MqD�qD��qD�qD�MqD��qD��>D�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�  A�  A���A���A�A�A�A�  A��mA���A���A��hA��DA��A�t�A�p�A�jA�\)A�VA�Q�A�O�A�K�A�M�A�I�A� �A�p�A�ffA�A���A��A���A�A��TA�dZA�{A��DA�C�A���A�`BA��A�hsA�bA��HA��uA�(�A��jA�Q�A��wA��;A�/A�
=A��-A��A��A�(�A���A�1'A��A��\A��A�~�A��A�r�A�ƨA��jA�33A�-A�jA��A��-A���A��A�33A���A�7LA���A��jA��yA�VA�n�A�"�A���A���A�l�A�1'A���A��A��A�~�A`BAx�AvbNAt�As�Ar��AqAo��Ann�AmO�Ak�;AkhsAk&�AjbNAe%A^JA\��A\VA[S�AY�7AV��ASl�AS
=AR�DARQ�AR$�ARJAQ�TAQ�AM�mAKXAJ�DAFE�A>��A;�-A;%A9��A7�wA5?}A4{A4bA41A37LA1
=A/S�A-�;A,�!A,  A+�A(�DA'�A&��A#��A"�A"(�A"JA!S�A ��A��A`BA?}A��AS�A��A��A;dAhsA�AoAK�AZAbNA
�`A
 �A�AA��A��A{A�A%AM�A�-A`BA�AjA  A�mA�FA+A E�@�\)@��@�%@�%@���@��@��@��P@���@�@���@��@�G�@��m@�v�@�@�w@�M�@�`B@��
@�@�@���@���@��@��@���@� �@띲@�M�@�@���@�1'@�l�@�^@�7L@�V@�9@�Z@��m@�F@�\)@��@�^@�&�@�j@��;@�ff@ݡ�@�bN@��H@�J@�Ĝ@�A�@�A�@��@�S�@�V@�V@�Q�@�S�@�J@Ѻ^@��@�I�@�1@Ϯ@�|�@�\)@�;d@�
=@Ο�@��T@�O�@̛�@�1@��
@˝�@�t�@��@�@Ɂ@���@ȋD@�A�@��@�b@�1@��@Ǖ�@�E�@š�@ŉ7@�O�@ă@�l�@°!@�-@�@�&�@��@�V@��`@�Z@��@�33@���@��@���@�9X@���@��@�~�@��@��7@���@��@��`@��/@���@��j@�bN@�1@��@���@��!@�^5@��@�X@�j@�9X@��@�ƨ@��P@�dZ@�@��R@�~�@�~�@�v�@�ff@���@�hs@�&�@���@��j@��D@�j@�9X@�1@��;@���@�"�@��H@���@�n�@�5?@���@�hs@�G�@���@��9@��u@�j@�I�@�A�@� �@�|�@��@�@��@�^5@��@���@��^@�hs@��u@�j@�I�@�  @���@�l�@�dZ@�"�@��@��\@�=q@���@��@��/@��@�j@�A�@� �@���@��;@��@�\)@�@��\@�-@�x�@�?}@�7L@���@��@�(�@���@��F@�l�@���@��#@�p�@�?}@��@�%@��@��j@��u@��w@���@���@��\@�v�@�=q@��@��@��/@���@���@��@��u@�j@�9X@���@�|�@�t�@�\)@�
=@��H@��H@�ȴ@���@��R@��\@�v�@���@�x�@�7L@�%@��j@�j@�  @��@�\)@�S�@�33@�
=@��y@��!@���@��+@�E�@��@��T@�@��h@�x�@�hs@�X@�G�@�7L@�&�@��@�Q�@�1@�K�@��y@�v�@�^5@�=q@��@���@��#@���@���@�x�@�G�@�V@��@�Ĝ@�z�@�9X@�b@��@��w@��F@��@���@��P@�|�@�\)@�"�@���@�ȴ@�ȴ@��\@�-@��@�x�@�O�@�&�@���@���@�9X@���@�@��@���@���@���@�ff@�V@�$�@��@���@��^@��h@�p�@�7L@���@��j@��D@�I�@�(�@���@�;d@��@�p�@�?}@�&�@���@�Ĝ@��9@��9@��9@���@��u@��D@��@� �@~ȴ@~�+@~V@~@}��@}p�@}?}@|�@|�@{�
@{�F@{S�@z�@z�\@z~�@zM�@y�@y��@x��@x�@xA�@w�;@w;d@v��@v�+@v$�@uO�@t�D@t9X@s��@s�
@sC�@r��@r�!@rn�@rJ@q�7@p�u@pb@oK�@n�y@n�+@n{@m��@m��@m�h@mp�@mO�@l�@l(�@kdZ@k"�@j~�@i�@h1'@h  @g�w@g�w@g�@g�@g|�@g+@f��@f��@f�@fȴ@f�+@fE�@e�@e��@e�-@e�@e�@d�@c��@c��@cC�@co@b��@b��@bn�@bJ@a�^@ax�@a&�@`bN@`b@_�w@_��@_;d@^ȴ@^V@]�T@]O�@\��@\�j@\�@\z�@[�m@[t�@["�@[@Z~�@Y�^@Y��@Y��@Yx�@Yhs@YX@Y7L@Y%@X�`@X�9@X�@X �@Xb@X  @X  @W�@W�;@W��@W��@W|�@W\)@W+@W
=@V�y@V�@V�R@V��@V��@V�+@VE�@V@U�T@U�@U/@T��@T�D@TZ@T(�@S��@S�m@SC�@R�\@R~�@RM�@Q��@Q�7@Qx�@QG�@P�`@P�9@PbN@O\)@Nv�@NE�@M��@M�h@M?}@MV@L�/@L��@Lz�@LZ@L9X@LI�@LI�@L9X@L1@K�m@K�F@KS�@J��@J~�@I��@Ihs@H�9@G�;@G��@G�P@G\)@GK�@G+@F��@E�h@EO�@D�/@Dj@C��@C��@C��@C��@CS�@C33@C"�@B�@B��@B�!@BM�@A��@Ahs@@�`@@bN@@ �@?�@?��@?\)@?�@>�y@>�@>�+@>$�@>@=��@=`B@=/@<�/@<I�@;�m@;S�@:��@:M�@:�@9G�@8�`@8��@8�9@8  @7�@6�@6�R@6��@6��@6�+@65?@5�T@5�T@5��@5��@5�h@5�@4�/@4��@4Z@3S�@3@2^5@1�^@1hs@1G�@1�@0bN@/�@/�w@/��@/��@/l�@/
=@/
=@.��@-�-@-O�@-?}@-O�@-�@,z�@,1@+�F@+�@+C�@+o@*�@*��@*-@)��@)�@)��@)hs@)7L@(��@(Ĝ@(bN@( �@'�;@'�w@'�@'|�@'�@&��@&v�@&5?@&@%�T@%�-@%�h@%�@%`B@%?}@%?}@%?}@$�j@$Z@$I�@$(�@$(�@$1@#��@#C�@#@"��@"�!@"��@"~�@"^5@"=q@!��@!��@!hs@!G�@ �u@ Q�@ 1'@   @�;@��@�@��@��@��@l�@�@ȴ@��@��@v�@ff@E�@{@�@�-@p�@/@V@�/@�D@Z@�@�
@dZ@�@��@~�@n�@-@��@&�@��@Ĝ@�u@1'@�@��@�@��@�P@|�@\)@\)@;d@��@E�@$�@5?@$�@�@�T@�T@�T@�T@@�h@p�@�@�j@�@9X@(�@�m@�F@��@��@��@��@��@dZ@��@��@~�@n�@n�@M�@�@��@��@�7@x�@hs@hs@hs@&�@%@��@�`@��@��@b@��@�P@l�@K�@
=@��@ȴ@ȴ@�R@��@�+@v�@v�@ff@V@E�@E�@@�@�@�/@�@j@Z@I�@9X@9X@9X@(�@�m@�
@ƨ@ƨ@ƨ@�F@�F@��@"�@
��@
�\@
M�@	��@	�#@	�^@	��@	��@	x�@	x�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�  A�  A���A���A�A�A�A�  A��mA���A���A��hA��DA��A�t�A�p�A�jA�\)A�VA�Q�A�O�A�K�A�M�A�I�A� �A�p�A�ffA�A���A��A���A�A��TA�dZA�{A��DA�C�A���A�`BA��A�hsA�bA��HA��uA�(�A��jA�Q�A��wA��;A�/A�
=A��-A��A��A�(�A���A�1'A��A��\A��A�~�A��A�r�A�ƨA��jA�33A�-A�jA��A��-A���A��A�33A���A�7LA���A��jA��yA�VA�n�A�"�A���A���A�l�A�1'A���A��A��A�~�A`BAx�AvbNAt�As�Ar��AqAo��Ann�AmO�Ak�;AkhsAk&�AjbNAe%A^JA\��A\VA[S�AY�7AV��ASl�AS
=AR�DARQ�AR$�ARJAQ�TAQ�AM�mAKXAJ�DAFE�A>��A;�-A;%A9��A7�wA5?}A4{A4bA41A37LA1
=A/S�A-�;A,�!A,  A+�A(�DA'�A&��A#��A"�A"(�A"JA!S�A ��A��A`BA?}A��AS�A��A��A;dAhsA�AoAK�AZAbNA
�`A
 �A�AA��A��A{A�A%AM�A�-A`BA�AjA  A�mA�FA+A E�@�\)@��@�%@�%@���@��@��@��P@���@�@���@��@�G�@��m@�v�@�@�w@�M�@�`B@��
@�@�@���@���@��@��@���@� �@띲@�M�@�@���@�1'@�l�@�^@�7L@�V@�9@�Z@��m@�F@�\)@��@�^@�&�@�j@��;@�ff@ݡ�@�bN@��H@�J@�Ĝ@�A�@�A�@��@�S�@�V@�V@�Q�@�S�@�J@Ѻ^@��@�I�@�1@Ϯ@�|�@�\)@�;d@�
=@Ο�@��T@�O�@̛�@�1@��
@˝�@�t�@��@�@Ɂ@���@ȋD@�A�@��@�b@�1@��@Ǖ�@�E�@š�@ŉ7@�O�@ă@�l�@°!@�-@�@�&�@��@�V@��`@�Z@��@�33@���@��@���@�9X@���@��@�~�@��@��7@���@��@��`@��/@���@��j@�bN@�1@��@���@��!@�^5@��@�X@�j@�9X@��@�ƨ@��P@�dZ@�@��R@�~�@�~�@�v�@�ff@���@�hs@�&�@���@��j@��D@�j@�9X@�1@��;@���@�"�@��H@���@�n�@�5?@���@�hs@�G�@���@��9@��u@�j@�I�@�A�@� �@�|�@��@�@��@�^5@��@���@��^@�hs@��u@�j@�I�@�  @���@�l�@�dZ@�"�@��@��\@�=q@���@��@��/@��@�j@�A�@� �@���@��;@��@�\)@�@��\@�-@�x�@�?}@�7L@���@��@�(�@���@��F@�l�@���@��#@�p�@�?}@��@�%@��@��j@��u@��w@���@���@��\@�v�@�=q@��@��@��/@���@���@��@��u@�j@�9X@���@�|�@�t�@�\)@�
=@��H@��H@�ȴ@���@��R@��\@�v�@���@�x�@�7L@�%@��j@�j@�  @��@�\)@�S�@�33@�
=@��y@��!@���@��+@�E�@��@��T@�@��h@�x�@�hs@�X@�G�@�7L@�&�@��@�Q�@�1@�K�@��y@�v�@�^5@�=q@��@���@��#@���@���@�x�@�G�@�V@��@�Ĝ@�z�@�9X@�b@��@��w@��F@��@���@��P@�|�@�\)@�"�@���@�ȴ@�ȴ@��\@�-@��@�x�@�O�@�&�@���@���@�9X@���@�@��@���@���@���@�ff@�V@�$�@��@���@��^@��h@�p�@�7L@���@��j@��D@�I�@�(�@���@�;d@��@�p�@�?}@�&�@���@�Ĝ@��9@��9@��9@���@��u@��D@��@� �@~ȴ@~�+@~V@~@}��@}p�@}?}@|�@|�@{�
@{�F@{S�@z�@z�\@z~�@zM�@y�@y��@x��@x�@xA�@w�;@w;d@v��@v�+@v$�@uO�@t�D@t9X@s��@s�
@sC�@r��@r�!@rn�@rJ@q�7@p�u@pb@oK�@n�y@n�+@n{@m��@m��@m�h@mp�@mO�@l�@l(�@kdZ@k"�@j~�@i�@h1'@h  @g�w@g�w@g�@g�@g|�@g+@f��@f��@f�@fȴ@f�+@fE�@e�@e��@e�-@e�@e�@d�@c��@c��@cC�@co@b��@b��@bn�@bJ@a�^@ax�@a&�@`bN@`b@_�w@_��@_;d@^ȴ@^V@]�T@]O�@\��@\�j@\�@\z�@[�m@[t�@["�@[@Z~�@Y�^@Y��@Y��@Yx�@Yhs@YX@Y7L@Y%@X�`@X�9@X�@X �@Xb@X  @X  @W�@W�;@W��@W��@W|�@W\)@W+@W
=@V�y@V�@V�R@V��@V��@V�+@VE�@V@U�T@U�@U/@T��@T�D@TZ@T(�@S��@S�m@SC�@R�\@R~�@RM�@Q��@Q�7@Qx�@QG�@P�`@P�9@PbN@O\)@Nv�@NE�@M��@M�h@M?}@MV@L�/@L��@Lz�@LZ@L9X@LI�@LI�@L9X@L1@K�m@K�F@KS�@J��@J~�@I��@Ihs@H�9@G�;@G��@G�P@G\)@GK�@G+@F��@E�h@EO�@D�/@Dj@C��@C��@C��@C��@CS�@C33@C"�@B�@B��@B�!@BM�@A��@Ahs@@�`@@bN@@ �@?�@?��@?\)@?�@>�y@>�@>�+@>$�@>@=��@=`B@=/@<�/@<I�@;�m@;S�@:��@:M�@:�@9G�@8�`@8��@8�9@8  @7�@6�@6�R@6��@6��@6�+@65?@5�T@5�T@5��@5��@5�h@5�@4�/@4��@4Z@3S�@3@2^5@1�^@1hs@1G�@1�@0bN@/�@/�w@/��@/��@/l�@/
=@/
=@.��@-�-@-O�@-?}@-O�@-�@,z�@,1@+�F@+�@+C�@+o@*�@*��@*-@)��@)�@)��@)hs@)7L@(��@(Ĝ@(bN@( �@'�;@'�w@'�@'|�@'�@&��@&v�@&5?@&@%�T@%�-@%�h@%�@%`B@%?}@%?}@%?}@$�j@$Z@$I�@$(�@$(�@$1@#��@#C�@#@"��@"�!@"��@"~�@"^5@"=q@!��@!��@!hs@!G�@ �u@ Q�@ 1'@   @�;@��@�@��@��@��@l�@�@ȴ@��@��@v�@ff@E�@{@�@�-@p�@/@V@�/@�D@Z@�@�
@dZ@�@��@~�@n�@-@��@&�@��@Ĝ@�u@1'@�@��@�@��@�P@|�@\)@\)@;d@��@E�@$�@5?@$�@�@�T@�T@�T@�T@@�h@p�@�@�j@�@9X@(�@�m@�F@��@��@��@��@��@dZ@��@��@~�@n�@n�@M�@�@��@��@�7@x�@hs@hs@hs@&�@%@��@�`@��@��@b@��@�P@l�@K�@
=@��@ȴ@ȴ@�R@��@�+@v�@v�@ff@V@E�@E�@@�@�@�/@�@j@Z@I�@9X@9X@9X@(�@�m@�
@ƨ@ƨ@ƨ@�F@�F@��@"�@
��@
�\@
M�@	��@	�#@	�^@	��@	��@	x�@	x�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�B�B�B�B�B�B�B�B�B�B�B�B�B{B{B{B{B{B{B{B�B�B{B{B�B�B'�B49B9XB8RB:^B;dB=qBC�BW
B^5BaHBaHB_;BhsB� B�bB��B��B��B��B��B��B�uB�1B|�By�Bq�BiyBcTB^5BVBD�B2-B+B$�B�BuB	7B��BƨB�BhsB@�B'�B{B
=BB
��B
�B
��B
�LB
��B
��B
�hB
~�B
w�B
l�B
ZB
ZB
`BB
iyB
^5B
N�B
7LB	��B	�mB	�B	��B	ƨB	�RB	�B	��B	��B	�hB	�PB	�7B	�B	r�B	YB	S�B	Q�B	M�B	I�B	B�B	5?B	1'B	/B	.B	-B	,B	)�B	%�B	�B	JB	B��B�#B��BǮB��B�XB�B�B�B�B�'B�B��B��B��B��B�{B�\B�JB�DB�%B�B�B�B� B}�B}�B{�B}�Bz�By�By�Bw�By�Bv�Bv�Bs�Bn�BgmBdZB\)B[#BZBXBT�BS�BR�BR�BQ�BQ�BO�BO�BN�BN�BL�BL�BK�BK�BM�BO�BP�BR�BaHBhsBm�BffBbNBiyBo�Br�Bt�Bw�Bt�Bq�Bp�Bn�Bl�Bv�Bu�B�B�%B�7B�\B��B��B��B��B��B�B�-B�3B�9B�LB�^B�dB�dB�jB�qB�wB�qB�wB�wB��B��BBĜBǮBɺB��B��B��B��B��B��B�B�
B�B�)B�/B�;B�NB�NB�`B�sB�B�B�B�B�B�B�B��B��B��B��B	  B	B	B	B	
=B	PB	\B	oB	uB	{B	{B	{B	{B	�B	�B	 �B	 �B	!�B	%�B	+B	/B	1'B	33B	6FB	7LB	7LB	7LB	:^B	>wB	?}B	B�B	E�B	J�B	M�B	O�B	S�B	W
B	XB	\)B	^5B	^5B	_;B	_;B	_;B	_;B	aHB	cTB	e`B	hsB	iyB	k�B	l�B	o�B	t�B	u�B	v�B	w�B	x�B	x�B	{�B	|�B	}�B	}�B	~�B	~�B	�B	�B	�+B	�1B	�7B	�=B	�DB	�JB	�PB	�PB	�VB	�hB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�'B	�'B	�3B	�RB	�XB	�XB	�dB	�qB	�qB	�qB	�}B	��B	��B	B	ŢB	ǮB	ǮB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�#B	�#B	�#B	�)B	�5B	�HB	�NB	�NB	�TB	�fB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
+B
+B
+B
+B
+B
1B
	7B

=B
PB
PB
VB
VB
\B
\B
bB
bB
bB
hB
hB
hB
oB
oB
oB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
#�B
$�B
$�B
$�B
$�B
$�B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
'�B
)�B
,B
,B
,B
,B
,B
-B
-B
-B
-B
-B
-B
-B
-B
.B
/B
/B
/B
0!B
0!B
0!B
0!B
1'B
2-B
2-B
2-B
2-B
33B
33B
33B
49B
49B
49B
5?B
6FB
6FB
6FB
7LB
7LB
7LB
6FB
7LB
8RB
8RB
8RB
8RB
9XB
:^B
:^B
:^B
:^B
;dB
<jB
<jB
=qB
=qB
>wB
>wB
>wB
>wB
>wB
?}B
>wB
?}B
?}B
@�B
@�B
A�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
F�B
F�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
S�B
VB
VB
VB
VB
W
B
W
B
W
B
W
B
W
B
XB
XB
YB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
[#B
[#B
\)B
\)B
[#B
\)B
\)B
[#B
\)B
\)B
\)B
\)B
]/B
^5B
^5B
_;B
`BB
`BB
`BB
`BB
`BB
`BB
aHB
bNB
bNB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
e`B
ffB
ffB
ffB
gmB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
jB
jB
jB
k�B
l�B
l�B
l�B
k�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
m�B
n�B
n�B
n�B
o�B
p�B
p�B
q�B
q�B
r�B
r�B
r�B
s�B
s�B
t�B
t�B
s�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
z�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
}�B
}�B
~�B
~�B
� B
� B
� B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�+B
�+B
�+B
�1B
�1B
�1B
�7B
�7B
�7B
�7B
�7B
�=B
�=B
�=B
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�PB
�PB
�PB
�PB
�PB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�\B
�\B
�\B
�\B
�\B
�\B
�\B
�bB
�bB
�bB
�bB
�bB
�bB
�bB
�bB
�hB
�hB
�hB
�hB
�hB
�hB
�oB
�oB
�oB
�uB
�uB
�uB
�uB
�{B
�{B
�{B
�{B
�{B
�{B
�{B
�{B
�{B
�{B
�{B
�{B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333B�B�B�B�B�B�B�B�B�B�B�B�B�B{B{B{B{B{B{B{B�B�B{B{B�B�B'�B49B9XB8RB:^B;dB=qBC�BW
B^5BaHBaHB_;BhsB� B�bB��B��B��B��B��B��B�uB�1B|�By�Bq�BiyBcTB^5BVBD�B2-B+B$�B�BuB	7B��BƨB�BhsB@�B'�B{B
=BB
��B
�B
��B
�LB
��B
��B
�hB
~�B
w�B
l�B
ZB
ZB
`BB
iyB
^5B
N�B
7LB	��B	�mB	�B	��B	ƨB	�RB	�B	��B	��B	�hB	�PB	�7B	�B	r�B	YB	S�B	Q�B	M�B	I�B	B�B	5?B	1'B	/B	.B	-B	,B	)�B	%�B	�B	JB	B��B�#B��BǮB��B�XB�B�B�B�B�'B�B��B��B��B��B�{B�\B�JB�DB�%B�B�B�B� B}�B}�B{�B}�Bz�By�By�Bw�By�Bv�Bv�Bs�Bn�BgmBdZB\)B[#BZBXBT�BS�BR�BR�BQ�BQ�BO�BO�BN�BN�BL�BL�BK�BK�BM�BO�BP�BR�BaHBhsBm�BffBbNBiyBo�Br�Bt�Bw�Bt�Bq�Bp�Bn�Bl�Bv�Bu�B�B�%B�7B�\B��B��B��B��B��B�B�-B�3B�9B�LB�^B�dB�dB�jB�qB�wB�qB�wB�wB��B��BBĜBǮBɺB��B��B��B��B��B��B�B�
B�B�)B�/B�;B�NB�NB�`B�sB�B�B�B�B�B�B�B��B��B��B��B	  B	B	B	B	
=B	PB	\B	oB	uB	{B	{B	{B	{B	�B	�B	 �B	 �B	!�B	%�B	+B	/B	1'B	33B	6FB	7LB	7LB	7LB	:^B	>wB	?}B	B�B	E�B	J�B	M�B	O�B	S�B	W
B	XB	\)B	^5B	^5B	_;B	_;B	_;B	_;B	aHB	cTB	e`B	hsB	iyB	k�B	l�B	o�B	t�B	u�B	v�B	w�B	x�B	x�B	{�B	|�B	}�B	}�B	~�B	~�B	�B	�B	�+B	�1B	�7B	�=B	�DB	�JB	�PB	�PB	�VB	�hB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�'B	�'B	�3B	�RB	�XB	�XB	�dB	�qB	�qB	�qB	�}B	��B	��B	B	ŢB	ǮB	ǮB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�#B	�#B	�#B	�)B	�5B	�HB	�NB	�NB	�TB	�fB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
+B
+B
+B
+B
+B
1B
	7B

=B
PB
PB
VB
VB
\B
\B
bB
bB
bB
hB
hB
hB
oB
oB
oB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
#�B
$�B
$�B
$�B
$�B
$�B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
'�B
)�B
,B
,B
,B
,B
,B
-B
-B
-B
-B
-B
-B
-B
-B
.B
/B
/B
/B
0!B
0!B
0!B
0!B
1'B
2-B
2-B
2-B
2-B
33B
33B
33B
49B
49B
49B
5?B
6FB
6FB
6FB
7LB
7LB
7LB
6FB
7LB
8RB
8RB
8RB
8RB
9XB
:^B
:^B
:^B
:^B
;dB
<jB
<jB
=qB
=qB
>wB
>wB
>wB
>wB
>wB
?}B
>wB
?}B
?}B
@�B
@�B
A�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
F�B
F�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
S�B
VB
VB
VB
VB
W
B
W
B
W
B
W
B
W
B
XB
XB
YB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
[#B
[#B
\)B
\)B
[#B
\)B
\)B
[#B
\)B
\)B
\)B
\)B
]/B
^5B
^5B
_;B
`BB
`BB
`BB
`BB
`BB
`BB
aHB
bNB
bNB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
e`B
ffB
ffB
ffB
gmB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
jB
jB
jB
k�B
l�B
l�B
l�B
k�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
m�B
n�B
n�B
n�B
o�B
p�B
p�B
q�B
q�B
r�B
r�B
r�B
s�B
s�B
t�B
t�B
s�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
z�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
}�B
}�B
~�B
~�B
� B
� B
� B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�+B
�+B
�+B
�1B
�1B
�1B
�7B
�7B
�7B
�7B
�7B
�=B
�=B
�=B
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�PB
�PB
�PB
�PB
�PB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�\B
�\B
�\B
�\B
�\B
�\B
�\B
�bB
�bB
�bB
�bB
�bB
�bB
�bB
�bB
�hB
�hB
�hB
�hB
�hB
�hB
�oB
�oB
�oB
�uB
�uB
�uB
�uB
�{B
�{B
�{B
�{B
�{B
�{B
�{B
�{B
�{B
�{B
�{B
�{B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA19c                                                                20230305004534  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8b                                                                20230304154746  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20230304154752  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20230304154755                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20230304154758  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20230304154758  QCF$                G�O�G�O�G�O�            8000JA  ARUP                                                                        20230304155812                      G�O�G�O�G�O�                