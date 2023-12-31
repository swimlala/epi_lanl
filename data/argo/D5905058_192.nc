CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-11-28T12:37:26Z creation;2019-11-28T12:37:32Z conversion to V3.1;2023-06-29T05:50:35Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         BPRIMARY|https://orcid.org/0000-0001-9150-6442|Kanako Sato, JAMSTEC        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7,   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7<   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7@   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7D   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7T   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7d   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7t   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7|   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8,   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    80   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    84   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     88   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8X   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8\   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8`   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9    CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        :    PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  :   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]@   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  a,   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �T   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �|   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �$   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �t   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �    HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20191128123726  20230705031507  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0675_192                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @��V�L��1   @��W�8�@7s�%��2�b��_��1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @@  @�33@�33A   A   A@  A`  A�  A�  A�33A�  A�  A�  A�  A�  B   B  B  B  B   B(ffB0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C�fC  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CO�fCR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�I�D�ff111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @Z�H@���@У�A�RA&�RAF�RAf�RA�\)A�\)A��\A�\)A�\)A�\)A�\)A�\)B�B	�B�B�B!�B*{B1�B9�BA�BI�BQ�BY�Ba�Bi�Bq�By�B��
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
C k�Ck�Ck�Ck�Ck�C
k�Ck�Ck�Ck�Ck�Ck�CQ�Ck�Ck�Ck�Ck�C k�C"k�C$k�C&k�C(k�C*k�C,k�C.k�C0k�C2k�C4k�C6k�C8k�C:k�C<k�C>k�C@k�CBk�CDk�CFk�CHk�CJk�CLk�CNk�CPQ�CRk�CTk�CVk�CXk�CZk�C\k�C^k�C`k�Cbk�Cdk�Cfk�Chk�Cjk�Clk�Cnk�Cpk�Crk�Ctk�Cvk�Cxk�Czk�C|k�C~k�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D��D�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD���D��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD�ФD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�P�D��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD�ФD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqDqD��qD�qD�MqDÍqD��qD�qD�MqDčqD��qD�qD�MqDōqD��qD�qD�MqDƍqD��qD�qD�MqDǍqD��qD�qD�MqDȍqD��qD�qD�MqDɍqD��qD�qD�MqDʍqD��qD�qD�MqDˍqD��qD�qD�MqD̍qD��qD�qD�MqD͍qD��qD�qD�MqD΍qD��qD�qD�MqDύqD��qD�qD�MqDЍqD��qD�qD�MqDэqD��qD�qD�MqDҍqD��qD�qD�MqDӍqD��qD�qD�MqDԍqD��qD�qD�MqDՍqD��qD�qD�MqD֍qD��qD�qD�MqD׍qD��qD�qD�MqD؍qD��qD�qD�MqDٍqD��qD�qD�MqDڍqD��qD�qD�MqDۍqD��qD�qD�MqD܍qD��qD�qD�MqDݍqD��qD�qD�MqDލqD��qD�qD�MqDߍqD��qD�qD�MqD��qD��qD�qD�MqD�qD��qD�qD�MqD�qD��qD�qD�MqD�qD��qD�qD�MqD�qD��qD�qD�MqD�qD��qD�qD�MqD�qD��qD�qD�MqD�qD��qD�qD�MqD�qD��qD�qD�MqD�qD��qD�qD�MqD�qD��qD�qD�MqD�qD��qD�qD�MqD�qD��qD�qD�MqD�qD��qD�qD�MqD�qD��qD�qD�MqD�qD��qD�qD�MqD��qD��qD�qD�MqD�qD��qD�qD�MqD�qD��qD�qD�MqD�qD��qD�qD�MqD�qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�W
D�s�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�-A�+A�(�A�-A�+A�(�A�&�A�(�A�(�A�+A�/A�/A��A��A� �A�"�A�"�A�"�A�$�A�$�A�&�A�(�A�&�A�(�A�(�A�+A�(�A�$�A�"�A� �A� �A�JA���A��A�dZA�G�A���A�JA��A�7LA���A�%A���A�C�A���A�x�A�r�A��DA��PA��PA���A�^5A��^A�{A��DA���A��A�t�A���A�\)A�7LA���A��\A�;dA���A�ƨA���A�ZA���A���A�K�A���A�\)A��uA���A�\)A���A�$�A��-A�/A��-A���A��FA�C�A��
A�A�A��yA�\)A�v�A���A�9XA��A�
=A�bNA��\A���A���A� �A��A�ƨA�/A��`A���A��-A�;dA���A�A�A�A��-A���A���A��A��DA��A���A��A{|�Ax�DAvffAr��Ar�+Arv�Aq�Aq�AoAm%AkO�Ag�7Ae�#AdjAb�yAaG�A^�A]?}A[�AZ�AX�HAVz�AUS�AT��AQ�
AOG�ALE�AJ$�AG�wAE��AD��AC|�A@z�A>��A>-A<�RA:ZA8~�A7�hA6��A5�A5S�A4�9A3|�A2�A0�`A0ffA0JA/"�A.ZA,ȴA*�jA)�A(�HA(bA'%A&r�A%��A$n�A#�A"�`A!�-A�AĜA��A��AhsA?}Az�A�!A{A�A�#A�A{A33A��AA�A��A  AS�A��A�mAt�A`BA�A�A��A+A
��A
ffA	�A5?AhsA+A�A�`A��A�A5?A�A�A|�A�PA"�@��P@���@�@�{@�  @�J@�`B@��@@�$�@�Z@�5?@�^@�9@��@��@�h@�j@�  @�\)@◍@��@�9@�  @�t�@���@���@�M�@�O�@܃@�r�@��m@�ƨ@�|�@��y@�x�@�V@�Ĝ@�1'@ׅ@�33@��H@ՙ�@�z�@ҟ�@��@Ӆ@�~�@�G�@�b@��
@���@��m@ˮ@�$�@��`@ȓu@��
@�S�@�S�@�|�@�  @Ǖ�@�ȴ@�~�@���@��@�Z@�/@�M�@ŉ7@ēu@�1@ÍP@��@�v�@���@�O�@��9@�Z@��m@�;d@���@��@��9@�bN@�Q�@��m@�\)@���@���@�O�@��/@�bN@���@�\)@�ȴ@�J@���@��7@�?}@��`@��9@��@���@��u@�Q�@��F@��@�ȴ@�^5@�n�@��+@���@��@�S�@���@��@�G�@�Q�@��F@�dZ@��P@�v�@��^@��@��@���@�A�@��@���@��P@��y@�=q@���@�hs@�&�@���@��D@�Q�@�Z@�Z@�Z@�Z@�A�@�1'@�1'@�1'@�b@��
@�33@��!@�^5@�V@�V@�M�@�E�@�{@���@�G�@�%@��/@���@�9X@���@�|�@��@�v�@��@��@�7L@��/@� �@��@�C�@�+@��@�
=@��H@��+@�5?@��#@��^@�X@��@��@���@�bN@�Z@��
@�+@��R@�ff@��^@��@� �@���@�M�@��@��-@�G�@��u@�ƨ@��@���@�p�@��/@�r�@�1@���@�|�@�dZ@�C�@���@�ȴ@���@�ff@�E�@��@��@��h@�p�@�O�@��@���@��D@�j@��@��m@���@��F@���@��P@�dZ@�;d@�"�@���@��y@���@�E�@��@�J@��@��#@��^@���@�?}@��@�Z@���@���@�t�@�K�@�;d@��@�o@���@���@�v�@�=q@�5?@�-@���@��T@���@���@��7@��@�x�@�p�@�`B@�G�@�V@��`@��j@���@�A�@�  @��;@�ƨ@��@���@��P@��@�t�@�l�@�C�@�@�ȴ@��+@�^5@�=q@�{@���@���@���@�?}@��@��D@�1@��@l�@
=@~��@~E�@~@}�-@}�@}�@|�j@|z�@|9X@{�m@{33@z��@z�!@z�\@z=q@y�#@yX@x�@xQ�@xb@w�;@w�@w�@vv�@u�@u?}@t�/@t��@tj@t�@s�m@s�@r��@r^5@r-@q�@q��@q�^@qx�@q�@pĜ@p1'@o�w@o+@n�+@n{@m��@mV@lI�@k33@kt�@ko@j~�@j�@i��@hĜ@g�;@g|�@f�y@f�+@f{@e`B@d�@dI�@d�@c�
@c�m@c�
@c�F@cS�@b��@b-@a�^@aX@a%@`bN@_��@_K�@^ȴ@^��@]�@]�@]p�@]/@\��@\z�@\(�@[�F@[�@["�@Z�H@Z�!@Z=q@Y��@Y�#@Y��@YX@Y7L@X��@X��@Xr�@Xb@W�P@W\)@V��@V�@V�R@V�+@Vv�@VV@V{@U�@U��@U�-@U�@U?}@T��@T1@S�
@S��@St�@S"�@R��@R~�@Rn�@R^5@R=q@R�@Q�@Q�#@Q��@Q&�@P��@P��@PQ�@P �@O�;@O�@O|�@O�@N��@Nȴ@N�+@NE�@M��@M`B@M�@L�@L��@L��@LZ@K��@K�F@KdZ@K@J�\@J=q@I��@IX@IG�@H��@H��@Hr�@HQ�@H  @G�w@G�w@G��@G|�@G+@F��@FV@F$�@F{@E��@EO�@D�/@D�@Dz�@DI�@D(�@C��@Cƨ@C��@CdZ@CC�@Co@C@B�H@B��@Bn�@BM�@A�@A�^@A��@Ahs@A�@@Ĝ@@bN@@1'@@b@@b@@  @?��@?|�@?|�@?\)@>��@>E�@=�T@=��@=�@=p�@=/@<�D@<j@<I�@;��@;�F@;��@;��@;t�@;33@:�@:��@:�\@:M�@:-@:-@:�@:J@:J@9�^@9x�@9x�@9G�@8�`@8r�@7�w@7�P@7K�@7
=@6��@6v�@65?@5�-@5/@4�j@4z�@3��@333@2~�@1�#@1�^@1��@1%@0�u@01'@0b@0  @0  @/��@/;d@.�@.v�@-@-O�@-V@,�j@,j@,(�@+�m@+�F@+t�@+�F@+dZ@*�\@*=q@*J@)�@)�^@)X@(��@(��@(�9@(�@(r�@( �@'�P@'K�@&�y@&�R@&v�@&E�@%�@%�@$�@$�j@$j@$Z@$9X@$(�@$�@#��@#dZ@#t�@#dZ@#33@"�H@"��@"�H@"��@"��@"�\@"n�@"n�@"^5@"M�@"-@!�@!��@!�7@!�7@!G�@!%@ �`@ ��@ �9@ �u@ �u@ �@ r�@ bN@ A�@ b@�;@�w@��@+@ȴ@ff@$�@�T@�@`B@p�@p�@?}@��@�/@�j@�@�D@I�@�@�
@ƨ@�F@�@S�@S�@C�@�H@��@��@�\@^5@^5@-@J@�#@��@��@�#@�#@��@��@��@x�@X@�`@�9@�u@r�@A�@b@�;@�w@��@l�@;d@�y@�R@��@��@ff@5?@{@@�@�T@��@@�h@p�@?}@��@�/@�j@�@�@�D@I�@(�@1@�m@�F@��@�@33@��@~�@M�@=q@��@��@x�@hs@G�@&�@��@�`@�`@Ĝ@Ĝ@�9@�9@��@A�@�@|�@l�@\)@+@�@��@��@v�@V@$�@@�T@�-@�h@p�@p�@O�@/@V@�j@j@1@�
@�F@�@S�@"�@
�@
�H@
��@
��@
�\@
n�@
M�@
=q@
-@
�@
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�-A�+A�(�A�-A�+A�(�A�&�A�(�A�(�A�+A�/A�/A��A��A� �A�"�A�"�A�"�A�$�A�$�A�&�A�(�A�&�A�(�A�(�A�+A�(�A�$�A�"�A� �A� �A�JA���A��A�dZA�G�A���A�JA��A�7LA���A�%A���A�C�A���A�x�A�r�A��DA��PA��PA���A�^5A��^A�{A��DA���A��A�t�A���A�\)A�7LA���A��\A�;dA���A�ƨA���A�ZA���A���A�K�A���A�\)A��uA���A�\)A���A�$�A��-A�/A��-A���A��FA�C�A��
A�A�A��yA�\)A�v�A���A�9XA��A�
=A�bNA��\A���A���A� �A��A�ƨA�/A��`A���A��-A�;dA���A�A�A�A��-A���A���A��A��DA��A���A��A{|�Ax�DAvffAr��Ar�+Arv�Aq�Aq�AoAm%AkO�Ag�7Ae�#AdjAb�yAaG�A^�A]?}A[�AZ�AX�HAVz�AUS�AT��AQ�
AOG�ALE�AJ$�AG�wAE��AD��AC|�A@z�A>��A>-A<�RA:ZA8~�A7�hA6��A5�A5S�A4�9A3|�A2�A0�`A0ffA0JA/"�A.ZA,ȴA*�jA)�A(�HA(bA'%A&r�A%��A$n�A#�A"�`A!�-A�AĜA��A��AhsA?}Az�A�!A{A�A�#A�A{A33A��AA�A��A  AS�A��A�mAt�A`BA�A�A��A+A
��A
ffA	�A5?AhsA+A�A�`A��A�A5?A�A�A|�A�PA"�@��P@���@�@�{@�  @�J@�`B@��@@�$�@�Z@�5?@�^@�9@��@��@�h@�j@�  @�\)@◍@��@�9@�  @�t�@���@���@�M�@�O�@܃@�r�@��m@�ƨ@�|�@��y@�x�@�V@�Ĝ@�1'@ׅ@�33@��H@ՙ�@�z�@ҟ�@��@Ӆ@�~�@�G�@�b@��
@���@��m@ˮ@�$�@��`@ȓu@��
@�S�@�S�@�|�@�  @Ǖ�@�ȴ@�~�@���@��@�Z@�/@�M�@ŉ7@ēu@�1@ÍP@��@�v�@���@�O�@��9@�Z@��m@�;d@���@��@��9@�bN@�Q�@��m@�\)@���@���@�O�@��/@�bN@���@�\)@�ȴ@�J@���@��7@�?}@��`@��9@��@���@��u@�Q�@��F@��@�ȴ@�^5@�n�@��+@���@��@�S�@���@��@�G�@�Q�@��F@�dZ@��P@�v�@��^@��@��@���@�A�@��@���@��P@��y@�=q@���@�hs@�&�@���@��D@�Q�@�Z@�Z@�Z@�Z@�A�@�1'@�1'@�1'@�b@��
@�33@��!@�^5@�V@�V@�M�@�E�@�{@���@�G�@�%@��/@���@�9X@���@�|�@��@�v�@��@��@�7L@��/@� �@��@�C�@�+@��@�
=@��H@��+@�5?@��#@��^@�X@��@��@���@�bN@�Z@��
@�+@��R@�ff@��^@��@� �@���@�M�@��@��-@�G�@��u@�ƨ@��@���@�p�@��/@�r�@�1@���@�|�@�dZ@�C�@���@�ȴ@���@�ff@�E�@��@��@��h@�p�@�O�@��@���@��D@�j@��@��m@���@��F@���@��P@�dZ@�;d@�"�@���@��y@���@�E�@��@�J@��@��#@��^@���@�?}@��@�Z@���@���@�t�@�K�@�;d@��@�o@���@���@�v�@�=q@�5?@�-@���@��T@���@���@��7@��@�x�@�p�@�`B@�G�@�V@��`@��j@���@�A�@�  @��;@�ƨ@��@���@��P@��@�t�@�l�@�C�@�@�ȴ@��+@�^5@�=q@�{@���@���@���@�?}@��@��D@�1@��@l�@
=@~��@~E�@~@}�-@}�@}�@|�j@|z�@|9X@{�m@{33@z��@z�!@z�\@z=q@y�#@yX@x�@xQ�@xb@w�;@w�@w�@vv�@u�@u?}@t�/@t��@tj@t�@s�m@s�@r��@r^5@r-@q�@q��@q�^@qx�@q�@pĜ@p1'@o�w@o+@n�+@n{@m��@mV@lI�@k33@kt�@ko@j~�@j�@i��@hĜ@g�;@g|�@f�y@f�+@f{@e`B@d�@dI�@d�@c�
@c�m@c�
@c�F@cS�@b��@b-@a�^@aX@a%@`bN@_��@_K�@^ȴ@^��@]�@]�@]p�@]/@\��@\z�@\(�@[�F@[�@["�@Z�H@Z�!@Z=q@Y��@Y�#@Y��@YX@Y7L@X��@X��@Xr�@Xb@W�P@W\)@V��@V�@V�R@V�+@Vv�@VV@V{@U�@U��@U�-@U�@U?}@T��@T1@S�
@S��@St�@S"�@R��@R~�@Rn�@R^5@R=q@R�@Q�@Q�#@Q��@Q&�@P��@P��@PQ�@P �@O�;@O�@O|�@O�@N��@Nȴ@N�+@NE�@M��@M`B@M�@L�@L��@L��@LZ@K��@K�F@KdZ@K@J�\@J=q@I��@IX@IG�@H��@H��@Hr�@HQ�@H  @G�w@G�w@G��@G|�@G+@F��@FV@F$�@F{@E��@EO�@D�/@D�@Dz�@DI�@D(�@C��@Cƨ@C��@CdZ@CC�@Co@C@B�H@B��@Bn�@BM�@A�@A�^@A��@Ahs@A�@@Ĝ@@bN@@1'@@b@@b@@  @?��@?|�@?|�@?\)@>��@>E�@=�T@=��@=�@=p�@=/@<�D@<j@<I�@;��@;�F@;��@;��@;t�@;33@:�@:��@:�\@:M�@:-@:-@:�@:J@:J@9�^@9x�@9x�@9G�@8�`@8r�@7�w@7�P@7K�@7
=@6��@6v�@65?@5�-@5/@4�j@4z�@3��@333@2~�@1�#@1�^@1��@1%@0�u@01'@0b@0  @0  @/��@/;d@.�@.v�@-@-O�@-V@,�j@,j@,(�@+�m@+�F@+t�@+�F@+dZ@*�\@*=q@*J@)�@)�^@)X@(��@(��@(�9@(�@(r�@( �@'�P@'K�@&�y@&�R@&v�@&E�@%�@%�@$�@$�j@$j@$Z@$9X@$(�@$�@#��@#dZ@#t�@#dZ@#33@"�H@"��@"�H@"��@"��@"�\@"n�@"n�@"^5@"M�@"-@!�@!��@!�7@!�7@!G�@!%@ �`@ ��@ �9@ �u@ �u@ �@ r�@ bN@ A�@ b@�;@�w@��@+@ȴ@ff@$�@�T@�@`B@p�@p�@?}@��@�/@�j@�@�D@I�@�@�
@ƨ@�F@�@S�@S�@C�@�H@��@��@�\@^5@^5@-@J@�#@��@��@�#@�#@��@��@��@x�@X@�`@�9@�u@r�@A�@b@�;@�w@��@l�@;d@�y@�R@��@��@ff@5?@{@@�@�T@��@@�h@p�@?}@��@�/@�j@�@�@�D@I�@(�@1@�m@�F@��@�@33@��@~�@M�@=q@��@��@x�@hs@G�@&�@��@�`@�`@Ĝ@Ĝ@�9@�9@��@A�@�@|�@l�@\)@+@�@��@��@v�@V@$�@@�T@�-@�h@p�@p�@O�@/@V@�j@j@1@�
@�F@�@S�@"�@
�@
�H@
��@
��@
�\@
n�@
M�@
=q@
-@
�@
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B7LB7LB7LB7LB7LB7LB7LB7LB7LB7LB5?B49B2-B2-B2-B2-B2-B2-B2-B2-B2-B33B2-B33B33B33B33B33B33B33B33B33B33B49B;dB;dB=qB@�B\)Bx�B�B��B�}BŢBɺB��B�TB�mBBbB�B!�B'�B-B5?BG�BO�BT�BZB\)B]/BbNBdZBffBl�Bq�Bp�Bq�Bu�Bx�Bx�Bw�Bv�Bu�Br�Bt�Bm�BhsBffBe`BaHB[#BP�BJ�BI�BG�BB�B?}B6FB)�B#�B�B�BbB	7B��B��B�B�fB��B�qB�RB�B��By�BXBK�B#�BbB
��B
�B
��B
�oB
u�B
R�B
C�B
$�B
	7B	��B	�/B	�B	�B	��B	��B	B	�'B	��B	�=B	|�B	r�B	e`B	]/B	J�B	B�B	:^B	/B	$�B	�B	VB	1B��B�yB�/B��BĜB�wB�jB�LB�B��B��B��B��B��B�{B�hB�VB�DB�7B�+B�%B�B~�B{�By�Bu�Bu�Bp�Bm�Bl�Bk�BjBjBhsBcTBbNB_;B_;BW
BH�BH�BL�BC�BB�BD�BA�B=qB>wB=qB;dB@�BC�B@�BF�B@�B<jB:^B9XB;dB=qB@�B@�B<jB;dB9XB7LB7LB;dB9XB6FB7LB7LB:^BB�B?}B?}B;dB9XB:^BE�BI�BA�B<jB8RB/B1'B49B<jBC�BM�BN�BP�BO�BN�BQ�BT�BW
BYBZB[#B\)B^5B_;B_;B`BBdZBcTBiyBjBiyBiyBjBjBk�Bl�Bm�Bp�Bp�Bq�Br�Bs�Bs�Bt�B�B�=B�+B�hB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�-B�3B�FB�jB�}BƨB��B�B�)B�#B�/B�)B�)B�/B�;B�HB�ZB�fB�yB�B�B�B�B�B�B��B��B	  B	  B	  B	B	B	
=B	
=B	
=B	PB	bB	bB	oB	�B	�B	!�B	&�B	'�B	'�B	+B	,B	-B	-B	0!B	2-B	5?B	7LB	B�B	D�B	E�B	F�B	I�B	H�B	H�B	J�B	M�B	M�B	N�B	N�B	P�B	Q�B	R�B	S�B	VB	W
B	[#B	_;B	bNB	cTB	gmB	m�B	r�B	t�B	u�B	u�B	v�B	w�B	w�B	w�B	w�B	x�B	z�B	}�B	�B	�B	�B	�B	�B	�B	�B	�B	�+B	�7B	�=B	�DB	�PB	�PB	�bB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�'B	�-B	�3B	�9B	�?B	�FB	�LB	�XB	�XB	�^B	�^B	�dB	�jB	�qB	�qB	�wB	�}B	��B	��B	ĜB	ŢB	ƨB	ƨB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�
B	�B	�B	�B	�B	�B	�#B	�#B	�)B	�/B	�/B	�/B	�/B	�/B	�/B	�5B	�5B	�;B	�BB	�BB	�NB	�TB	�TB	�TB	�ZB	�ZB	�ZB	�ZB	�ZB	�ZB	�`B	�fB	�mB	�sB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
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
+B
+B
+B
+B
1B
	7B
	7B

=B
DB
DB
DB
JB
JB
PB
VB
\B
\B
\B
\B
VB
\B
\B
bB
bB
bB
hB
hB
oB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
 �B
!�B
!�B
!�B
!�B
"�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
+B
,B
,B
,B
,B
,B
,B
,B
,B
-B
-B
-B
-B
/B
/B
/B
/B
/B
/B
/B
0!B
0!B
0!B
0!B
0!B
1'B
1'B
1'B
2-B
2-B
2-B
33B
33B
33B
33B
49B
49B
49B
49B
49B
5?B
5?B
5?B
5?B
5?B
5?B
6FB
6FB
6FB
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
8RB
8RB
8RB
8RB
9XB
:^B
:^B
:^B
:^B
:^B
;dB
;dB
;dB
;dB
<jB
<jB
<jB
<jB
=qB
=qB
=qB
=qB
>wB
?}B
?}B
@�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
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
E�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
K�B
L�B
M�B
N�B
N�B
N�B
N�B
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
Q�B
Q�B
Q�B
Q�B
R�B
R�B
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
W
B
W
B
XB
XB
XB
XB
YB
YB
YB
YB
YB
YB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
[#B
\)B
\)B
\)B
\)B
\)B
\)B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
`BB
`BB
`BB
aHB
bNB
bNB
bNB
cTB
cTB
cTB
dZB
e`B
e`B
e`B
e`B
e`B
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
hsB
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
jB
jB
jB
jB
jB
jB
k�B
jB
jB
k�B
k�B
jB
jB
jB
jB
jB
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l�B
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
m�B
m�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
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
q�B
p�B
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
q�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B72B72B72B72B72B72B72B72B72B72B5%B49B1�B2B2B2B1�B2B2B1�B2B3B2B3B3B3B3B3B3B33B3MB3MB3hB5%B<B<�BAoBIlBd�B|PB��B�LB��BƨB�DBҽB�B�6B�BhB�B# B)*B.IB72BH�BQ BV9BZ�B\�B^Bb�BeBg�Bm�BrBqvBsMBwLBy�By�By	BxlBwLBt9Bw�Bn�BiyBg�Bf�Bc:B]BQ�BK�BJ�BH�BC�BA;B7�B+6B%,B!B
B�B
=B��B�$B�oB�yB�"B�]B��B�'B��B}qBZ�BP�B%�B�B[B
�IB
�B
�$B
y�B
U�B
G�B
($B
B	��B	�dB	�eB	��B	�B	бB	��B	��B	��B	�dB	~�B	t�B	g�B	_�B	L~B	D3B	<B	1AB	'RB	�B	�B	�B	 B��B��BѝB��B��B�]B�xB��B�*B��B�zB��B��B��B� B�B�JB��B��B�_B��B�B}B{Bw�Bw�Bq�Bn�Bm�Bl�BkQBk�Bi�BdZBc�Ba-Bb4BYeBIRBJ#BN�BDBC�BFYBBB=�B>�B>wB<�BAoBDMBAUBH1BAoB=<B;0B:B;�B=�BA;BA B<�B<B9�B7�B88B="B:B6zB7�B7�B;�BC�B@4B@�B<�B:B:�BF�BK^BCaB>�B:^B0oB2-B4�B<�BDMBN�BO�BQ�BP.BOvBRoBU�BW�BYBZ�B[�B\�B^�B_�B_�B`�Bd�BcTBi�BkBi�BiyBj�BjBk�Bl�BnIBp�Bp�Bq�Br�Bs�BtBu�B��B��B��B�NB�=B�vB�=B�B��B��B��B��B�`B�B�2B��B��B��B��B�IB�|B��B��B��B��B�B�PBچBܒB�qB�dB�]B�xB�dBߊB�B�tB�B��B�B��B�;B�B�B��B�*B�cB	 OB	 4B	 4B	;B	aB	
XB	
�B	
�B	PB	.B	}B	�B	�B	�B	!�B	&�B	(
B	($B	+B	,WB	-B	,�B	/�B	1�B	4�B	7B	B�B	D�B	E�B	GB	I�B	H�B	H�B	KDB	N"B	M�B	N�B	N�B	Q B	Q�B	R�B	T,B	VSB	WYB	[=B	_VB	bNB	cnB	gmB	mwB	r|B	t�B	u�B	u�B	v�B	w�B	w�B	w�B	w�B	x�B	{0B	~B	�B	��B	��B	��B	��B	�B	�-B	�MB	�+B	�7B	�=B	�^B	�jB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�CB	�cB	�IB	�IB	�}B	�}B	�6B	�*B	�mB	��B	��B	�B	�:B	�-B	�-B	�NB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�/B	�B	�B	�B	�B	�?B	�FB	�LB	�>B	�>B	�DB	�DB	�JB	�PB	�VB	�VB	�BB	�cB	��B	��B	āB	ňB	ƎB	ƎB	ǔB	ȴB	��B	��B	�6B	� B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�B	�B	�B	�	B	�	B	�)B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�BB	�\B	�NB	�:B	�:B	�:B	�@B	�&B	�&B	�@B	�@B	�ZB	�FB	�fB	�mB	�XB	�XB	�_B	�_B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
�B
�B
�B
�B
B
3B
B
B
B
B
B
B
+B
B
1B
	7B
	B

=B
)B
DB
^B
dB
dB
B
<B
\B
\B
\B
vB
pB
\B
\B
HB
bB
}B
hB
hB
TB
@B
FB
gB
mB
�B
�B
�B
mB
�B
�B
�B
yB
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
�B
xB
�B
~B
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
!�B
!�B
!�B
"�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
+B
+�B
+�B
+�B
+�B
+�B
+�B
+�B
+�B
-B
-B
-B
-B
/ B
/ B
/ B
/ B
/ B
/ B
/ B
0B
/�B
/�B
0B
0!B
1'B
1B
1B
2B
2B
2B
3B
3B
3B
3B
4B
4B
4B
4B
4B
5B
5%B
5%B
5%B
5B
5%B
6+B
6+B
6+B
6+B
72B
72B
72B
72B
88B
88B
8B
88B
88B
88B
8B
88B
8RB
9XB
:DB
:DB
:DB
:DB
:DB
;dB
;JB
;JB
;JB
<PB
<6B
<6B
<PB
=VB
=VB
=VB
=VB
>]B
?HB
?HB
@OB
AUB
AoB
AoB
BuB
B[B
BuB
B�B
B�B
C�B
CaB
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
E�B
F�B
F�B
G�B
GzB
G�B
G�B
G�B
H�B
HfB
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
K�B
L�B
M�B
N�B
N�B
N�B
N�B
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
Q�B
Q�B
Q�B
RB
R�B
R�B
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
U�B
U�B
U�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
W�B
W�B
W�B
W�B
X�B
X�B
X�B
X�B
X�B
X�B
Y�B
Y�B
Y�B
ZB
ZB
ZB
Y�B
Z�B
\B
\B
\B
\B
\B
[�B
\�B
\�B
^B
^B
^B
^B
^B
^B
^B
^B
^B
^B
^B
^B
^B
^5B
]�B
_!B
_!B
_!B
_B
_!B
`'B
`B
`'B
aB
b4B
bB
bB
c B
c B
c B
d@B
e,B
eFB
eFB
e`B
eFB
f2B
fLB
fLB
fLB
gRB
gRB
gmB
gRB
gRB
gRB
gRB
h>B
hXB
hXB
h>B
h>B
h>B
h>B
iDB
i*B
i_B
i_B
i_B
i_B
i_B
jeB
jeB
j0B
jKB
jeB
jeB
jeB
jKB
kkB
jeB
jeB
kkB
kkB
jeB
jeB
jeB
jKB
jeB
kkB
kkB
kQB
kkB
kkB
kkB
kQB
lWB
lqB
lqB
m]B
m]B
mwB
m�B
m�B
mwB
m]B
mwB
mwB
m�B
n}B
ncB
ncB
n}B
n}B
o�B
oiB
o�B
o�B
o�B
oiB
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
q�B
poB
p�B
q�B
qvB
qvB
q�B
q�B
qvB
qvB
qvB
qvB
qv111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.42(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201912050044402019120500444020191205004440202306231719152023062317191520230623171915201912060113512019120601135120191206011351  JA  ARFMdecpA19c                                                                20191128213720  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20191128123726  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20191128123728  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20191128123729  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20191128123730  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20191128123730  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20191128123730  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20191128123730  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20191128123732  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20191128123732                      G�O�G�O�G�O�                JA  ARUP                                                                        20191128125445                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20191128153326  CV  JULD            G�O�G�O�F�z�                JM  ARGQJMQC2.0                                                                 20191128153326  CV  JULD_LOCATION   G�O�G�O�F�z�                JM  ARGQJMQC2.0                                                                 20191128153326  CV  LONGITUDE       G�O�G�O����                JM  ARCAJMQC2.0                                                                 20191204154440  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20191204154440  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20191205161351  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20230623081915  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20230705031507                      G�O�G�O�G�O�                