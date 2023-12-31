CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-01-27T12:43:14Z creation;2022-01-27T12:43:16Z conversion to V3.1      
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
resolution        =���   axis      Z        |  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     |  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \X   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  `8   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �l   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  �L   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     |  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  �$   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  ̠   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �L   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �L   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �L   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �L   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �x   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �|   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20220127124314  20220127125222  5905219                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            7906                            051216                          846 @ٴ����1   @ٴ�-��.@4W�O�;d�dQ����1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B��B��B   B(  B0  B8  B@  BH  BP  BX  B`  Bg��Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B���B�  B���B���B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C �C  C  C  C  C
  C  C  C  C  C  C�fC  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C7�fC:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� DfD�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D fD � D!  D!� D"  D"� D#  D#�fD$fD$� D%  D%� D&  D&y�D'  D'� D(  D(� D)  D)� D*  D*y�D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<y�D=  D=� D>  D>�fD?  D?� D@  D@� DA  DA� DB  DBy�DB��DCy�DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL�fDM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]y�D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk�fDl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}fD}�fD~  D~� D  Dy�D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�<�D�|�D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D��3D��3D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�|�D���D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�3D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ Dμ�D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ DӼ�D�  D�@ DԀ D�� D�  D�@ DՀ Dռ�D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D�|�D�� D�3D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D��3D�  D�@ D܀ Dܼ�D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�<�D�|�D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D��D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @���@�p�A�RA&�RAF�RAf�RA�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)B�B	�BG�BG�B!�B)�B1�B9�BA�BI�BQ�BY�Ba�BiG�Bq�By�B��
B��
B��
B��
B��
B��
B��
B��
B�
=B��
B���B��
B���B���B���B���B��
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
B��
C �Ck�Ck�Ck�Ck�C
k�Ck�Ck�Ck�Ck�Ck�CQ�Ck�Ck�Ck�Ck�C k�C"k�C$k�C&k�C(k�C*k�C,k�C.k�C0k�C2k�C4k�C6k�C8Q�C:k�C<k�C>k�C@k�CBk�CDk�CFk�CHk�CJk�CLk�CNk�CPk�CRk�CTk�CVk�CXk�CZk�C\k�C^k�C`k�Cbk�Cdk�Cfk�Chk�Cjk�Clk�Cnk�Cpk�Crk�Ctk�Cvk�Cxk�Czk�C|k�C~k�C�5�C�5�C�5�C�5�C�5�C�5�C�(�C�(�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�(�C�5�C�5�C�B�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�(�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�(�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�D �D ��D�D��D�D��D�D�GD�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D!GD�GD�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D !GD ��D!�D!��D"�D"��D#�D#�GD$!GD$��D%�D%��D&�D&�{D'�D'��D(�D(��D)�D)��D*�D*�{D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<�{D=�D=��D>�D>�GD?�D?��D@�D@��DA�DA��DB�DB�{DC{DC�{DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL�GDM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]�{D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk�GDl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}!GD}�GD~�D~��D�D�{D�qD�MqD��qD�ФD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��>D�qD�MqD��qD��qD�qD�J>D��>D��qD�qD�MqD��qD��qD�qD�J>D��qD��qD�qD�J>D��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD�ФD��D�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��>D��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�
>D�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD�ФD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�J>D��qD��qD�qD�MqD���D�ФD�qD�MqD��qD�ФD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD��D�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��>D�qD�MqD��qD��qD�qD�MqD���D��qD�qD�MqD��qD��qD��D�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD��D�P�D��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��>D��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��>D��qD�qD�MqD��qD��qD�qD�MqD��>D��>D�qD�MqD��qD��qD�qD�MqDqD��qD�qD�MqDÍqD��qD�qD�MqDčqD��qD�qD�MqDōqD��qD��D�MqDƍqD��qD�qD�MqDǍqD��qD�qD�MqDȍqD��qD�qD�MqDɍqD��qD�qD�MqDʍqD��qD�qD�MqDˍqD��qD�qD�MqD̍qD��qD�qD�MqD͍qD��qD�qD�MqD΍qD��>D�qD�MqDύqD��qD�qD�MqDЍqD��qD�qD�MqDэqD��qD�qD�MqDҍqD��qD�qD�MqDӍqD��>D�qD�MqDԍqD��qD�qD�MqDՍqD��>D�qD�MqD֍qD��qD�qD�MqD׍qD��qD�qD�MqD؊>D��qD��D�MqDٍqD��qD�qD�MqDڍqD��qD�qD�MqDۍqD�ФD�qD�MqD܍qD��>D�qD�MqDݍqD��qD�qD�MqDލqD��qD�qD�J>Dߊ>D��qD�qD�MqD��qD��qD�qD�MqD�qD��qD�qD�MqD�qD��qD�qD�MqD�qD��qD�qD�MqD�qD��qD�
>D�MqD�qD��qD�qD�MqD�qD��qD�qD�MqD�qD��qD�qD�MqD�qD��qD�qD�MqD�qD��qD�qD�MqD�qD��qD�qD�MqD�qD��qD��D�MqD�qD��qD�qD�MqD�qD��qD�qD�MqD�qD��qD�qD�MqD�qD��qD�qD�MqD��qD��qD�qD�MqD�qD��qD�qD�MqD�qD��>D�qD�MqD�qD��qD�qD�J>D�qD��qD�qD�MqD��qD��qD�
>D�MqD��qD��qD�qD�MqD��qD��qD��q1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�M�A�M�A�O�A�O�A�O�A�Q�A�S�A�S�A�VA�VA�VA�\)A�^5A�dZA�dZA�l�AЧ�A���A�;dAЕ�A��Aϴ9A�VAͲ-A͋DA���A̲-A�O�A�M�A�ƨAʉ7A�{Aɩ�A�^5A�
=A��`A�ȴAȡ�AȁA�O�A��
A�A�bNA�+A��A��TA�~�A�VAĝ�A��TA�hsA�hsA�jA��A��mA�^5A�&�A���A��A��9A�1A�33A���A���A�&�A���A��A�K�A�oA�9XA���A���A���A��A�x�A� �A���A��FA���A��HA�hsA�+A��#A�z�A��A�dZA�?}A���A�|�A��A��+A�  A���A�"�A���A���A��A��A��TA�ffA���A��A�XA��A���A�9XA�G�A��HA��uA��+A��!A}?}A|�Az�Ay%Aw&�AvI�AvAuG�At�DAp�Ak?}Ai&�Ah�`AhQ�Ae�TAc�PA`ȴA]hsAZ�AW33AU"�AT �AR~�APM�ANZALr�AJbNAG?}AD��AC
=AB1AAoA?�A=O�A<�\A;�
A;oA:$�A9�hA9�A9C�A8jA7oA6�\A5O�A4��A3�^A2JA/K�A.E�A-�7A+�hA*  A)�A(jA&�!A$�A"�A"��A"jA!�wA!|�A �Ar�A�A�#A��A�7A��A��Ar�A33A9XA��A�A+A\)A�uA�
A`BAAM�A��A"�A
�A
�RA
�\A
bA�A�A��AC�A9XA��A�-AK�A
=A��A�RA�A��A5?A�AVA $�@�ff@�@���@��@��@�b@�
=@���@��@�@��@�t�@�33@�ȴ@���@�@��#@�Z@�33@�V@�`B@�A�@�\@�u@�"�@ᙚ@���@��u@�S�@݉7@�5?@�@ف@ؼj@ְ!@��@���@�K�@��T@���@��@�l�@�o@���@Ο�@�E�@Ͳ-@�hs@�V@�I�@˥�@�dZ@��@�ff@ɡ�@ȓu@ǥ�@��y@�M�@�$�@�J@�p�@���@�Ĝ@�z�@�j@�1'@� �@� �@��;@�;d@°!@�n�@�@���@�ƨ@�;d@���@���@�5?@�@��#@��-@�hs@��@���@���@�1@�@���@��R@��@�`B@��`@�j@��@�l�@���@���@���@�n�@�$�@���@���@���@�x�@�7L@��D@�9X@���@�@�~�@���@�`B@��`@�Ĝ@�A�@��@��P@�t�@�t�@�t�@�\)@�C�@���@�ff@�$�@��@���@�O�@�&�@���@��@��D@�9X@��@��m@��@�\)@��@��y@��\@��@��^@�x�@�?}@��@�(�@�dZ@�\)@�C�@�+@�
=@���@���@��@��u@���@��@�?}@�hs@�x�@�hs@���@���@���@�C�@��H@���@�n�@�=q@��@��@�{@�J@�@��@���@��^@�X@��@��`@�j@�A�@�9X@�9X@���@���@�;d@�
=@��H@���@�M�@���@�7L@��`@�Q�@��9@�z�@��@�S�@�"�@��@��H@��R@��+@�@�&�@��u@�9X@���@�;d@�o@���@��@���@��-@��7@��@��@��9@���@��u@�j@� �@��m@���@���@�K�@���@�V@�5?@���@��7@�G�@���@�j@��@�t�@�C�@��H@��R@���@��\@�V@�5?@�$�@�{@��@���@�x�@�?}@�7L@�/@��@��@�bN@�Q�@�I�@� �@��@�dZ@�S�@�C�@�C�@���@���@���@���@���@���@���@�@��@�
=@�ȴ@��!@��\@�~�@�~�@�=q@��@��^@��h@�p�@�`B@�7L@��@���@���@��D@�r�@�9X@���@�ƨ@���@�;d@��y@���@��+@���@��^@��7@�x�@�X@�?}@��@�%@��@���@�r�@��@��;@���@��F@��P@�dZ@�C�@��@���@���@��!@���@���@�ff@�-@��T@���@�/@�V@��@���@��@�z�@�Z@�A�@�1'@�b@�@~��@}�@}�@}/@|z�@{�F@{"�@z�\@z-@y�^@x�9@xr�@xA�@xb@w�@wl�@w
=@v��@vV@u��@up�@t��@tZ@tI�@s�
@s33@s@rn�@q�^@q�7@pĜ@pQ�@p  @o�@n��@nv�@n@m�-@m`B@l��@l�D@lZ@lI�@l�@k�F@kdZ@k@jn�@jM�@i��@i��@ix�@h�`@g��@gK�@g
=@f��@fE�@f{@e@e/@d�@d�@c"�@b�H@b�!@a�^@a7L@`��@`�@`1'@_�;@_|�@_l�@_\)@_;d@^�@^�+@^5?@]�@]��@]�h@]�@]/@]�@\��@\j@\(�@[�m@[t�@[33@Z�H@Z=q@Y��@Y7L@X�`@X�u@XQ�@X �@Xb@W��@W�P@Wl�@V��@V��@Vv�@V5?@U�@U��@U�@T�/@T�j@Tz�@T(�@S��@S�F@S��@SdZ@S"�@R��@R^5@R=q@Q�@Q��@Q7L@P�u@Pb@O�;@O�@O�P@O|�@O;d@N�@N��@Nv�@M@M��@M�@M?}@L�@L�/@L��@L��@LZ@K�F@KS�@Ko@K@J�@J^5@I��@Ihs@I&�@H��@H�`@HĜ@H�@HA�@H  @G�@G|�@G+@F��@F�@Fff@F$�@F@E�T@E�@EV@D�@DZ@D1@C�@CC�@C@B��@A�#@Ahs@A�@@��@@r�@@A�@@ �@?�w@?�P@?;d@>v�@>5?@=�T@<��@<z�@<I�@<1@;�F@;t�@;33@:��@:~�@:�@9X@9%@8��@8�u@7�@7�@7�P@7l�@7K�@7�@6��@6{@5@5@5�-@5/@4�D@4j@4I�@3�m@3�
@3ƨ@3�F@3��@3��@3�@3t�@3t�@3dZ@3C�@333@2�H@2��@2�\@2n�@2�@1�#@1��@1�^@1�^@1�7@1&�@1�@1%@0A�@/�;@/�P@/;d@/�@.��@.�@.ȴ@.��@.��@.�+@.V@.@-��@-`B@-O�@-O�@,��@,�@,�D@,z�@,I�@+�
@+��@+33@+"�@*�@*�!@*~�@*n�@*J@)�7@)hs@)7L@)&�@(��@(�`@(��@( �@'��@'��@'�P@'|�@'l�@&ȴ@&V@&E�@&5?@&$�@&@%�T@%��@%�h@%�@%�@%p�@%p�@%`B@%V@$�/@$I�@$�@$�@$1@#�m@#�F@#o@"�!@"n�@"-@!�#@!�^@!�7@!X@!�@ ��@ Ĝ@ bN@   @��@��@K�@��@�@�+@$�@�h@`B@/@�@�/@�/@��@j@I�@1@�m@ƨ@�F@��@C�@��@~�@=q@��@��@X@�@�`@�9@�u@A�@b@�@|�@;d@+@�@
=@��@�R@��@�+@5?@$�@{@@p�@/@V@��@�@��@��@�D@j@�@�
@�F@t�@S�@C�@33@33@��@n�@M�@-@��@��@��@�7@X@�@��@�9@��@r�@bN@A�@b@�P@\)@��@��@��@��@��@�+@V@5?@{@�T@�-@�@V@�@�/@�/@��@�j@�@��@��@�D@j@(�@�m@ƨ@�F@��@�F1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�M�A�M�A�O�A�O�A�O�A�Q�A�S�A�S�A�VA�VA�VA�\)A�^5A�dZA�dZA�l�AЧ�A���A�;dAЕ�A��Aϴ9A�VAͲ-A͋DA���A̲-A�O�A�M�A�ƨAʉ7A�{Aɩ�A�^5A�
=A��`A�ȴAȡ�AȁA�O�A��
A�A�bNA�+A��A��TA�~�A�VAĝ�A��TA�hsA�hsA�jA��A��mA�^5A�&�A���A��A��9A�1A�33A���A���A�&�A���A��A�K�A�oA�9XA���A���A���A��A�x�A� �A���A��FA���A��HA�hsA�+A��#A�z�A��A�dZA�?}A���A�|�A��A��+A�  A���A�"�A���A���A��A��A��TA�ffA���A��A�XA��A���A�9XA�G�A��HA��uA��+A��!A}?}A|�Az�Ay%Aw&�AvI�AvAuG�At�DAp�Ak?}Ai&�Ah�`AhQ�Ae�TAc�PA`ȴA]hsAZ�AW33AU"�AT �AR~�APM�ANZALr�AJbNAG?}AD��AC
=AB1AAoA?�A=O�A<�\A;�
A;oA:$�A9�hA9�A9C�A8jA7oA6�\A5O�A4��A3�^A2JA/K�A.E�A-�7A+�hA*  A)�A(jA&�!A$�A"�A"��A"jA!�wA!|�A �Ar�A�A�#A��A�7A��A��Ar�A33A9XA��A�A+A\)A�uA�
A`BAAM�A��A"�A
�A
�RA
�\A
bA�A�A��AC�A9XA��A�-AK�A
=A��A�RA�A��A5?A�AVA $�@�ff@�@���@��@��@�b@�
=@���@��@�@��@�t�@�33@�ȴ@���@�@��#@�Z@�33@�V@�`B@�A�@�\@�u@�"�@ᙚ@���@��u@�S�@݉7@�5?@�@ف@ؼj@ְ!@��@���@�K�@��T@���@��@�l�@�o@���@Ο�@�E�@Ͳ-@�hs@�V@�I�@˥�@�dZ@��@�ff@ɡ�@ȓu@ǥ�@��y@�M�@�$�@�J@�p�@���@�Ĝ@�z�@�j@�1'@� �@� �@��;@�;d@°!@�n�@�@���@�ƨ@�;d@���@���@�5?@�@��#@��-@�hs@��@���@���@�1@�@���@��R@��@�`B@��`@�j@��@�l�@���@���@���@�n�@�$�@���@���@���@�x�@�7L@��D@�9X@���@�@�~�@���@�`B@��`@�Ĝ@�A�@��@��P@�t�@�t�@�t�@�\)@�C�@���@�ff@�$�@��@���@�O�@�&�@���@��@��D@�9X@��@��m@��@�\)@��@��y@��\@��@��^@�x�@�?}@��@�(�@�dZ@�\)@�C�@�+@�
=@���@���@��@��u@���@��@�?}@�hs@�x�@�hs@���@���@���@�C�@��H@���@�n�@�=q@��@��@�{@�J@�@��@���@��^@�X@��@��`@�j@�A�@�9X@�9X@���@���@�;d@�
=@��H@���@�M�@���@�7L@��`@�Q�@��9@�z�@��@�S�@�"�@��@��H@��R@��+@�@�&�@��u@�9X@���@�;d@�o@���@��@���@��-@��7@��@��@��9@���@��u@�j@� �@��m@���@���@�K�@���@�V@�5?@���@��7@�G�@���@�j@��@�t�@�C�@��H@��R@���@��\@�V@�5?@�$�@�{@��@���@�x�@�?}@�7L@�/@��@��@�bN@�Q�@�I�@� �@��@�dZ@�S�@�C�@�C�@���@���@���@���@���@���@���@�@��@�
=@�ȴ@��!@��\@�~�@�~�@�=q@��@��^@��h@�p�@�`B@�7L@��@���@���@��D@�r�@�9X@���@�ƨ@���@�;d@��y@���@��+@���@��^@��7@�x�@�X@�?}@��@�%@��@���@�r�@��@��;@���@��F@��P@�dZ@�C�@��@���@���@��!@���@���@�ff@�-@��T@���@�/@�V@��@���@��@�z�@�Z@�A�@�1'@�b@�@~��@}�@}�@}/@|z�@{�F@{"�@z�\@z-@y�^@x�9@xr�@xA�@xb@w�@wl�@w
=@v��@vV@u��@up�@t��@tZ@tI�@s�
@s33@s@rn�@q�^@q�7@pĜ@pQ�@p  @o�@n��@nv�@n@m�-@m`B@l��@l�D@lZ@lI�@l�@k�F@kdZ@k@jn�@jM�@i��@i��@ix�@h�`@g��@gK�@g
=@f��@fE�@f{@e@e/@d�@d�@c"�@b�H@b�!@a�^@a7L@`��@`�@`1'@_�;@_|�@_l�@_\)@_;d@^�@^�+@^5?@]�@]��@]�h@]�@]/@]�@\��@\j@\(�@[�m@[t�@[33@Z�H@Z=q@Y��@Y7L@X�`@X�u@XQ�@X �@Xb@W��@W�P@Wl�@V��@V��@Vv�@V5?@U�@U��@U�@T�/@T�j@Tz�@T(�@S��@S�F@S��@SdZ@S"�@R��@R^5@R=q@Q�@Q��@Q7L@P�u@Pb@O�;@O�@O�P@O|�@O;d@N�@N��@Nv�@M@M��@M�@M?}@L�@L�/@L��@L��@LZ@K�F@KS�@Ko@K@J�@J^5@I��@Ihs@I&�@H��@H�`@HĜ@H�@HA�@H  @G�@G|�@G+@F��@F�@Fff@F$�@F@E�T@E�@EV@D�@DZ@D1@C�@CC�@C@B��@A�#@Ahs@A�@@��@@r�@@A�@@ �@?�w@?�P@?;d@>v�@>5?@=�T@<��@<z�@<I�@<1@;�F@;t�@;33@:��@:~�@:�@9X@9%@8��@8�u@7�@7�@7�P@7l�@7K�@7�@6��@6{@5@5@5�-@5/@4�D@4j@4I�@3�m@3�
@3ƨ@3�F@3��@3��@3�@3t�@3t�@3dZ@3C�@333@2�H@2��@2�\@2n�@2�@1�#@1��@1�^@1�^@1�7@1&�@1�@1%@0A�@/�;@/�P@/;d@/�@.��@.�@.ȴ@.��@.��@.�+@.V@.@-��@-`B@-O�@-O�@,��@,�@,�D@,z�@,I�@+�
@+��@+33@+"�@*�@*�!@*~�@*n�@*J@)�7@)hs@)7L@)&�@(��@(�`@(��@( �@'��@'��@'�P@'|�@'l�@&ȴ@&V@&E�@&5?@&$�@&@%�T@%��@%�h@%�@%�@%p�@%p�@%`B@%V@$�/@$I�@$�@$�@$1@#�m@#�F@#o@"�!@"n�@"-@!�#@!�^@!�7@!X@!�@ ��@ Ĝ@ bN@   @��@��@K�@��@�@�+@$�@�h@`B@/@�@�/@�/@��@j@I�@1@�m@ƨ@�F@��@C�@��@~�@=q@��@��@X@�@�`@�9@�u@A�@b@�@|�@;d@+@�@
=@��@�R@��@�+@5?@$�@{@@p�@/@V@��@�@��@��@�D@j@�@�
@�F@t�@S�@C�@33@33@��@n�@M�@-@��@��@��@�7@X@�@��@�9@��@r�@bN@A�@b@�P@\)@��@��@��@��@��@�+@V@5?@{@�T@�-@�@V@�@�/@�/@��@�j@�@��@��@�D@j@(�@�m@ƨ@�F@��@�F1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B$�B%�B%�B%�B%�B%�B%�B%�B%�B&�B&�B&�B&�B(�B(�B-BE�BT�B�=B��B��B��B�9B�B�B��B��B��B�B�RB�}BȴB��B��BĜBǮB��B��BǮBƨB��B�mB��B��B��BB��B��B�B�BƨB��B�5B�yB�B�B�sB��B��BBbB{B�B�BhBJB1B1B\BoB�B'�B$�BVB��B��B��B��B�B�`B�;B�B��B��BÖB�LB��B��B�B�9B�!B�B��B�\Bz�Bm�BbNBS�BG�B7LB�B{B
=B
��B
�yB
ƨB
�3B
��B
�\B
�B
o�B
N�B
D�B
9XB
0!B
"�B
�B
�B
{B
PB	��B	�B	ȴB	ŢB	ÖB	�FB	�B	��B	�DB	|�B	l�B	^5B	W
B	O�B	E�B	<jB	2-B	.B	�B	�B	\B	DB	1B	B��B��B��B�B�B�B�B�B�sB�TB�HB�/B�B�
B��B��BƨBÖB�}B�^B�RB�?B�FB�'B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�B�B�B�3B�RB�dB�jB�jB�jB�qBBĜBĜBƨBǮBǮBȴB��B��B��B��B��B��B��B��B�B�#B�#B�#B�)B�BB�BB�TB�fB�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B	  B	B	B	1B	
=B	PB	VB	VB	hB	uB	{B	{B	{B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	%�B	'�B	)�B	,B	/B	1'B	49B	6FB	8RB	9XB	9XB	:^B	>wB	C�B	C�B	D�B	I�B	L�B	N�B	P�B	R�B	R�B	S�B	W
B	XB	ZB	]/B	`BB	aHB	cTB	dZB	e`B	iyB	k�B	n�B	r�B	v�B	y�B	~�B	�B	�B	�B	�B	�1B	�7B	�=B	�=B	�DB	�JB	�VB	�bB	�hB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�3B	�9B	�9B	�LB	�LB	�FB	�-B	�-B	�-B	�?B	�jB	�}B	��B	ÖB	ĜB	ĜB	ƨB	ƨB	ƨB	ƨB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	�B	�#B	�#B	�5B	�BB	�;B	�;B	�fB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
  B
  B
B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
1B
	7B

=B

=B

=B
DB
JB
JB
PB
PB
JB
JB
PB
VB
\B
\B
\B
\B
bB
hB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
 �B
!�B
 �B
 �B
 �B
!�B
"�B
"�B
"�B
"�B
"�B
#�B
#�B
#�B
$�B
$�B
%�B
%�B
%�B
%�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
)�B
)�B
+B
,B
,B
,B
-B
-B
-B
-B
-B
.B
.B
.B
/B
/B
/B
/B
0!B
0!B
0!B
1'B
1'B
1'B
2-B
2-B
33B
33B
33B
33B
33B
49B
49B
49B
49B
5?B
5?B
5?B
6FB
6FB
6FB
6FB
7LB
6FB
7LB
6FB
7LB
6FB
7LB
7LB
7LB
7LB
8RB
8RB
8RB
8RB
8RB
9XB
9XB
;dB
;dB
<jB
<jB
<jB
<jB
<jB
<jB
>wB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
?}B
?}B
@�B
@�B
@�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
D�B
D�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
M�B
M�B
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
O�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
T�B
T�B
T�B
T�B
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
W
B
W
B
XB
XB
XB
XB
XB
XB
YB
XB
XB
YB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
^5B
]/B
^5B
]/B
]/B
]/B
^5B
]/B
]/B
^5B
^5B
^5B
_;B
_;B
_;B
`BB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
bNB
bNB
cTB
dZB
dZB
dZB
dZB
e`B
ffB
ffB
ffB
ffB
ffB
gmB
gmB
gmB
gmB
gmB
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
iyB
iyB
jB
jB
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
k�B
l�B
l�B
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
n�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
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
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
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
z�B
z�B
z�B
z�B
{�B
{�B
{�B
{�B
|�B
|�B
|�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
~�B
~�B
� B
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
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
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
�+B
�+B
�+B
�1B
�+B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�7B
�1B
�71111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B$�B%�B%�B%�B%�B%�B%�B%�B%�B&�B&�B&�B&�B(�B(�B-BE�BT�B�=B��B��B��B�9B�B�B��B��B��B�B�RB�}BȴB��B��BĜBǮB��B��BǮBƨB��B�mB��B��B��BB��B��B�B�BƨB��B�5B�yB�B�B�sB��B��BBbB{B�B�BhBJB1B1B\BoB�B'�B$�BVB��B��B��B��B�B�`B�;B�B��B��BÖB�LB��B��B�B�9B�!B�B��B�\Bz�Bm�BbNBS�BG�B7LB�B{B
=B
��B
�yB
ƨB
�3B
��B
�\B
�B
o�B
N�B
D�B
9XB
0!B
"�B
�B
�B
{B
PB	��B	�B	ȴB	ŢB	ÖB	�FB	�B	��B	�DB	|�B	l�B	^5B	W
B	O�B	E�B	<jB	2-B	.B	�B	�B	\B	DB	1B	B��B��B��B�B�B�B�B�B�sB�TB�HB�/B�B�
B��B��BƨBÖB�}B�^B�RB�?B�FB�'B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�B�B�B�3B�RB�dB�jB�jB�jB�qBBĜBĜBƨBǮBǮBȴB��B��B��B��B��B��B��B��B�B�#B�#B�#B�)B�BB�BB�TB�fB�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B	  B	B	B	1B	
=B	PB	VB	VB	hB	uB	{B	{B	{B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	%�B	'�B	)�B	,B	/B	1'B	49B	6FB	8RB	9XB	9XB	:^B	>wB	C�B	C�B	D�B	I�B	L�B	N�B	P�B	R�B	R�B	S�B	W
B	XB	ZB	]/B	`BB	aHB	cTB	dZB	e`B	iyB	k�B	n�B	r�B	v�B	y�B	~�B	�B	�B	�B	�B	�1B	�7B	�=B	�=B	�DB	�JB	�VB	�bB	�hB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�3B	�9B	�9B	�LB	�LB	�FB	�-B	�-B	�-B	�?B	�jB	�}B	��B	ÖB	ĜB	ĜB	ƨB	ƨB	ƨB	ƨB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	�B	�#B	�#B	�5B	�BB	�;B	�;B	�fB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
  B
  B
B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
1B
	7B

=B

=B

=B
DB
JB
JB
PB
PB
JB
JB
PB
VB
\B
\B
\B
\B
bB
hB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
 �B
!�B
 �B
 �B
 �B
!�B
"�B
"�B
"�B
"�B
"�B
#�B
#�B
#�B
$�B
$�B
%�B
%�B
%�B
%�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
)�B
)�B
+B
,B
,B
,B
-B
-B
-B
-B
-B
.B
.B
.B
/B
/B
/B
/B
0!B
0!B
0!B
1'B
1'B
1'B
2-B
2-B
33B
33B
33B
33B
33B
49B
49B
49B
49B
5?B
5?B
5?B
6FB
6FB
6FB
6FB
7LB
6FB
7LB
6FB
7LB
6FB
7LB
7LB
7LB
7LB
8RB
8RB
8RB
8RB
8RB
9XB
9XB
;dB
;dB
<jB
<jB
<jB
<jB
<jB
<jB
>wB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
?}B
?}B
@�B
@�B
@�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
D�B
D�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
M�B
M�B
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
O�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
T�B
T�B
T�B
T�B
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
W
B
W
B
XB
XB
XB
XB
XB
XB
YB
XB
XB
YB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
^5B
]/B
^5B
]/B
]/B
]/B
^5B
]/B
]/B
^5B
^5B
^5B
_;B
_;B
_;B
`BB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
bNB
bNB
cTB
dZB
dZB
dZB
dZB
e`B
ffB
ffB
ffB
ffB
ffB
gmB
gmB
gmB
gmB
gmB
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
iyB
iyB
jB
jB
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
k�B
l�B
l�B
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
n�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
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
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
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
z�B
z�B
z�B
z�B
{�B
{�B
{�B
{�B
|�B
|�B
|�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
~�B
~�B
� B
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
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
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
�+B
�+B
�+B
�1B
�+B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�7B
�1B
�71111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA19c                                                                20220127214151  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220127124314  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220127124315  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20220127124315  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20220127124315  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20220127124315  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20220127124315  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20220127124315  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20220127124316  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20220127124316                      G�O�G�O�G�O�                JA  ARUP                                                                        20220127125222                      G�O�G�O�G�O�                