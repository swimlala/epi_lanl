CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2021-01-29T03:47:03Z creation;2021-01-29T03:47:05Z conversion to V3.1;2023-06-29T05:47:23Z update;     
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
resolution        =���   axis      Z        H  :   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  IL   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     H  M    PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \h   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     H  `<   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     H  sX   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     H  �t   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     H  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     H  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     H  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     H  �<   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ڄ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20210129034703  20230705041505  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL              $A   JA  I2_0675_292                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @�Z8�=р1   @�Z;����@6���)_�b�����1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0ffB8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CK�fCN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Diy�Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D��3D�3D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D�� D�  D�@ D� D�� D�  D�@ D� D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�p�@�p�A�RA&�RAF�RAf�RA�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)B�B	�B�B�B!�B)�B2{B9�BA�BI�BQ�BY�Ba�Bi�Bq�By�B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B�
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
C k�Ck�Ck�Ck�Ck�C
k�Ck�Ck�Ck�Ck�Ck�Ck�Ck�Ck�Ck�Ck�C k�C"k�C$k�C&k�C(k�C*k�C,k�C.k�C0k�C2k�C4k�C6k�C8k�C:k�C<k�C>k�C@k�CBk�CDk�CFk�CHk�CJk�CLQ�CNk�CPk�CRk�CTk�CVk�CXk�CZk�C\k�C^k�C`k�Cbk�Cdk�Cfk�Chk�Cjk�Clk�Cnk�Cpk�Crk�Ctk�Cvk�Cxk�Czk�C|k�C~k�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di�{Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D��D�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqDqD��qD�qD�MqDÍqD��qD�qD�MqDčqD��qD�qD�MqDōqD��qD�qD�MqDƍqD��qD�qD�MqDǍqD��qD�qD�MqDȍqD��qD�qD�MqDɍqD��qD�qD�MqDʍqD��qD�qD�MqDˍqD��qD�qD�MqD̍qD��qD�qD�MqD͍qD��qD�qD�MqD΍qD��qD�qD�MqDύqD��qD�qD�MqDЍqD��qD�qD�MqDэqD��qD�qD�MqDҍqD��qD�qD�MqDӍqD��qD�qD�MqDԍqD��qD�qD�MqDՍqD��qD�qD�MqD֍qD��qD�qD�MqD׍qD��qD�qD�MqD؍qD��qD�qD�MqDٍqD��qD�qD�MqDڍqD��qD�qD�MqDۍqD��qD�qD�MqD܍qD�ФD��D�MqDݍqD��qD�qD�MqDލqD��qD�qD�MqDߍqD��qD�qD�MqD��qD��qD�qD�MqD�qD��qD�qD�MqD�qD��qD�qD�MqD㐤D��qD�qD�MqD�qD��qD�qD�MqD�qD�ФD�qD�MqD�qD��qD�qD�MqD�qD��qD�qD�MqD�qD��qD�qD�MqD�qD��qD�qD�MqD�qD��qD�qD�MqD�qD��qD�qD�MqD�qD��qD�qD�MqD�qD��qD�qD�MqD�qD��qD�qD�MqD�qD��qD�qD�MqD��qD��qD�qD�MqD�qD��qD�qD�MqD�qD��qD�qD�MqD�qD��qD�qD�MqD���D��q111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�1'A�1'A�1'A�1'A�33A�33A�33A�5?A�5?A�5?A�5?A�7LA�9XA�9XA�;dA�;dA�=qA�=qA�?}A�?}A�A�A�A�A�A�A�A�A�C�A�C�A�E�A�E�A�E�A�E�A�E�A�E�A�E�A�G�A�G�A�I�A�I�A�G�A�E�A�(�A���A���A��A��RA��;A��
A��A�  A�-A�  A���A��A�?}A�ƨA��wA�bNA���A���A��HA�oA��yA��A��!A���A��A���A��A��^A�|�A�`BA�n�A�XA�VA���A���A�bNA���A�jA�-A��`A���A�5?A�^5A�E�A��A�A�G�A���A�~�A�/A�^5A��A���A��A�oA�ZA�=qA��A�
=A��A��A�E�A�z�A�$�A��9A�|�A��9A��A��7A�hsA�\)A�E�A��\A��`A�;A|��Az-Ax��Av��Ar��AnffAk��Ahv�Ac33A`��A]x�A\ffA[��AX��AV�DAT��AQAO
=AL �AJ��AI;dAG��AF��ADE�AB�AA��A@(�A=��A<��A:�\A9��A8�/A6v�A5?}A2��A0�`A/�A/&�A.��A.jA.A�A-x�A*��A($�A'dZA&ȴA%+A$bNA#"�A!dZA �RA z�A��A1A�A�
A��A
=A��A�A%AQ�AQ�A��AhsAoA��A�wA��AA�AƨA�PA��A;dA�-A��AO�A
�!A
 �A	G�A�\A�#A��AS�A�PA��A{AhsA�!At�A ~�@���@�x�@�/@���@��@�V@�O�@���@�%@�I�@�@���@�x�@�X@�/@�Z@�"�@��@�%@�A�@��;@�!@�?}@�  @���@� �@�\)@�~�@�@��`@��@ߥ�@ݑh@�
=@��#@�+@���@ԋD@�j@�|�@Ѳ-@Ѳ-@�G�@�Q�@� �@�K�@�n�@��#@��`@�1@�\)@�b@Ƈ+@�J@�`B@ě�@�  @��
@��@+@�5?@�X@�t�@�{@�@���@���@���@�7L@�z�@�dZ@�@�ƨ@�"�@�@���@��R@���@�%@�n�@�-@���@�`B@�%@��/@���@�(�@�S�@��@��\@��+@�=q@��T@���@���@�X@�V@��/@���@�bN@��@��R@�E�@�@��h@�hs@�/@�V@���@��`@�Q�@��P@�;d@���@�M�@��@�@�?}@���@�bN@��@���@�S�@�@��@��H@��@��@��!@�ff@��#@���@��7@�x�@��9@�(�@�  @�t�@��@�
=@��@�~�@�V@�$�@��@��h@�X@��@��j@��@��u@�bN@�A�@�9X@�b@�ƨ@��P@�;d@��@��!@�ff@�5?@���@��#@���@��7@�X@�&�@��`@��j@��@�z�@�Z@�9X@��@���@���@�|�@��@��y@��!@��\@�ff@�J@�x�@�X@�/@�%@���@���@�z�@�Q�@�(�@��@��w@��P@�S�@�"�@�@��@���@��+@�n�@�-@��@���@���@���@��@�p�@�/@��`@���@��j@�z�@��@��@��@��P@�"�@���@���@�~�@�-@���@��@�G�@�V@��9@��@�j@�bN@�Q�@�I�@�b@��@��
@�ƨ@��@�|�@�@��y@�ȴ@��R@���@���@���@�v�@�^5@��@���@���@��h@��@�G�@��@��/@���@���@���@�Ĝ@��9@�Z@�1@��m@���@�l�@�S�@��@��@��R@��+@�^5@�-@���@�X@�&�@��@��u@�9X@��@���@�|�@�\)@�C�@�
=@�ȴ@�~�@�=q@�@�@��-@�hs@�7L@�&�@���@��D@�(�@�@�P@\)@;d@~��@}�T@}p�@|j@{��@{33@z��@z-@y�@y��@y&�@x�9@x�@xA�@x �@w�w@wl�@wK�@v�y@v$�@u�h@t�@tZ@s�m@s��@sS�@s33@r�@r��@r�!@rJ@q��@qhs@q�@pr�@pbN@p  @o��@o|�@ol�@ol�@o
=@n��@n$�@m��@m�h@mp�@l��@lj@l(�@k��@k�
@k�@k33@j��@j-@i��@i��@ihs@iX@i7L@h��@h�9@hb@g�w@g�@g|�@gl�@gK�@g+@f�@f�+@fE�@e�-@e/@d�@dI�@d�@c��@c�m@c��@c"�@b�H@b�\@bn�@b^5@b�@a�@a�#@a��@ax�@aG�@a%@`Ĝ@`�u@` �@_K�@^ȴ@^�+@^E�@]�h@\�@\�/@\��@\Z@\I�@\I�@\I�@\(�@[�m@[��@[t�@[C�@Z�H@Z^5@Y��@X�`@X  @W
=@Vff@V5?@V$�@U��@U?}@T��@T�j@T��@Tz�@Tj@TZ@T�@Sƨ@SC�@R^5@R=q@R�@RJ@Q�@QX@Q�@P�u@PA�@P �@O�;@O|�@Ol�@O\)@O
=@N��@NE�@N@M�-@Mp�@M?}@L��@L�j@L�@L��@L��@L�D@Lz�@LZ@L9X@L1@K�m@K�
@K�
@K�F@K��@K�@KS�@J�H@Jn�@J=q@I�^@I7L@H��@H �@G�;@G�@G�P@Gl�@Gl�@F�y@FV@FE�@F5?@F@E�T@E�-@E�@E?}@E/@EV@D�j@Dz�@DI�@D(�@C�m@C�F@C�@CS�@C@Bn�@A�#@A��@Ax�@@��@@�@@Q�@@b@?�w@?|�@?\)@?
=@>�R@>v�@>E�@=��@=p�@<��@<��@<j@<1@;��@;�m@;�F@;��@;dZ@;@:n�@:^5@:M�@:M�@:M�@:M�@:=q@:�@9�#@9�^@9��@9hs@9&�@8Ĝ@8r�@8Q�@8 �@7��@7\)@6��@6�@6�+@6ff@6V@6@5p�@5�@4��@4��@4��@4j@4Z@4I�@4I�@4(�@41@3�m@3�F@3��@3t�@3C�@3o@2��@2�\@2~�@2M�@1�@1��@1��@1��@1hs@1&�@0Ĝ@0bN@0 �@/�;@/��@/l�@/\)@/;d@.��@.�R@.�+@.ff@.E�@.{@-�@-�T@-��@-��@-O�@-?}@,��@,��@,z�@,1@+ƨ@+�F@+��@+33@*�@*��@*�!@*��@*~�@*n�@*M�@*�@)��@)X@)&�@(Ĝ@(�u@(Q�@(1'@'�;@'�w@'|�@'+@'
=@'
=@&��@&�y@&��@&@%��@%�@%O�@$��@$�@$�/@$��@$�j@$�j@$��@$z�@$I�@$1@#�@#C�@#"�@#@"�!@"�\@"~�@"n�@"=q@!��@!��@!��@!x�@!hs@!7L@ ��@ �`@ Ĝ@ �@ r�@ 1'@�;@�;@��@�w@K�@��@ȴ@��@��@ff@5?@{@@�h@�@?}@�/@j@9X@�@1@ƨ@��@�F@S�@o@�@��@��@^5@-@��@��@�7@7L@�@��@�9@r�@Q�@b@�@�;@�;@�@+@��@��@ff@{@@�@@�@O�@V@�/@j@9X@1@�
@ƨ@�F@t�@33@C�@33@@��@��@~�@-@�@�#@��@x�@X@G�@�@�`@�u@Q�@1'@�;@��@�P@��@�P@|�@l�@K�@+@��@�R@��@��@��@�+@�+@v�@v�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�1'A�1'A�1'A�1'A�33A�33A�33A�5?A�5?A�5?A�5?A�7LA�9XA�9XA�;dA�;dA�=qA�=qA�?}A�?}A�A�A�A�A�A�A�A�A�C�A�C�A�E�A�E�A�E�A�E�A�E�A�E�A�E�A�G�A�G�A�I�A�I�A�G�A�E�A�(�A���A���A��A��RA��;A��
A��A�  A�-A�  A���A��A�?}A�ƨA��wA�bNA���A���A��HA�oA��yA��A��!A���A��A���A��A��^A�|�A�`BA�n�A�XA�VA���A���A�bNA���A�jA�-A��`A���A�5?A�^5A�E�A��A�A�G�A���A�~�A�/A�^5A��A���A��A�oA�ZA�=qA��A�
=A��A��A�E�A�z�A�$�A��9A�|�A��9A��A��7A�hsA�\)A�E�A��\A��`A�;A|��Az-Ax��Av��Ar��AnffAk��Ahv�Ac33A`��A]x�A\ffA[��AX��AV�DAT��AQAO
=AL �AJ��AI;dAG��AF��ADE�AB�AA��A@(�A=��A<��A:�\A9��A8�/A6v�A5?}A2��A0�`A/�A/&�A.��A.jA.A�A-x�A*��A($�A'dZA&ȴA%+A$bNA#"�A!dZA �RA z�A��A1A�A�
A��A
=A��A�A%AQ�AQ�A��AhsAoA��A�wA��AA�AƨA�PA��A;dA�-A��AO�A
�!A
 �A	G�A�\A�#A��AS�A�PA��A{AhsA�!At�A ~�@���@�x�@�/@���@��@�V@�O�@���@�%@�I�@�@���@�x�@�X@�/@�Z@�"�@��@�%@�A�@��;@�!@�?}@�  @���@� �@�\)@�~�@�@��`@��@ߥ�@ݑh@�
=@��#@�+@���@ԋD@�j@�|�@Ѳ-@Ѳ-@�G�@�Q�@� �@�K�@�n�@��#@��`@�1@�\)@�b@Ƈ+@�J@�`B@ě�@�  @��
@��@+@�5?@�X@�t�@�{@�@���@���@���@�7L@�z�@�dZ@�@�ƨ@�"�@�@���@��R@���@�%@�n�@�-@���@�`B@�%@��/@���@�(�@�S�@��@��\@��+@�=q@��T@���@���@�X@�V@��/@���@�bN@��@��R@�E�@�@��h@�hs@�/@�V@���@��`@�Q�@��P@�;d@���@�M�@��@�@�?}@���@�bN@��@���@�S�@�@��@��H@��@��@��!@�ff@��#@���@��7@�x�@��9@�(�@�  @�t�@��@�
=@��@�~�@�V@�$�@��@��h@�X@��@��j@��@��u@�bN@�A�@�9X@�b@�ƨ@��P@�;d@��@��!@�ff@�5?@���@��#@���@��7@�X@�&�@��`@��j@��@�z�@�Z@�9X@��@���@���@�|�@��@��y@��!@��\@�ff@�J@�x�@�X@�/@�%@���@���@�z�@�Q�@�(�@��@��w@��P@�S�@�"�@�@��@���@��+@�n�@�-@��@���@���@���@��@�p�@�/@��`@���@��j@�z�@��@��@��@��P@�"�@���@���@�~�@�-@���@��@�G�@�V@��9@��@�j@�bN@�Q�@�I�@�b@��@��
@�ƨ@��@�|�@�@��y@�ȴ@��R@���@���@���@�v�@�^5@��@���@���@��h@��@�G�@��@��/@���@���@���@�Ĝ@��9@�Z@�1@��m@���@�l�@�S�@��@��@��R@��+@�^5@�-@���@�X@�&�@��@��u@�9X@��@���@�|�@�\)@�C�@�
=@�ȴ@�~�@�=q@�@�@��-@�hs@�7L@�&�@���@��D@�(�@�@�P@\)@;d@~��@}�T@}p�@|j@{��@{33@z��@z-@y�@y��@y&�@x�9@x�@xA�@x �@w�w@wl�@wK�@v�y@v$�@u�h@t�@tZ@s�m@s��@sS�@s33@r�@r��@r�!@rJ@q��@qhs@q�@pr�@pbN@p  @o��@o|�@ol�@ol�@o
=@n��@n$�@m��@m�h@mp�@l��@lj@l(�@k��@k�
@k�@k33@j��@j-@i��@i��@ihs@iX@i7L@h��@h�9@hb@g�w@g�@g|�@gl�@gK�@g+@f�@f�+@fE�@e�-@e/@d�@dI�@d�@c��@c�m@c��@c"�@b�H@b�\@bn�@b^5@b�@a�@a�#@a��@ax�@aG�@a%@`Ĝ@`�u@` �@_K�@^ȴ@^�+@^E�@]�h@\�@\�/@\��@\Z@\I�@\I�@\I�@\(�@[�m@[��@[t�@[C�@Z�H@Z^5@Y��@X�`@X  @W
=@Vff@V5?@V$�@U��@U?}@T��@T�j@T��@Tz�@Tj@TZ@T�@Sƨ@SC�@R^5@R=q@R�@RJ@Q�@QX@Q�@P�u@PA�@P �@O�;@O|�@Ol�@O\)@O
=@N��@NE�@N@M�-@Mp�@M?}@L��@L�j@L�@L��@L��@L�D@Lz�@LZ@L9X@L1@K�m@K�
@K�
@K�F@K��@K�@KS�@J�H@Jn�@J=q@I�^@I7L@H��@H �@G�;@G�@G�P@Gl�@Gl�@F�y@FV@FE�@F5?@F@E�T@E�-@E�@E?}@E/@EV@D�j@Dz�@DI�@D(�@C�m@C�F@C�@CS�@C@Bn�@A�#@A��@Ax�@@��@@�@@Q�@@b@?�w@?|�@?\)@?
=@>�R@>v�@>E�@=��@=p�@<��@<��@<j@<1@;��@;�m@;�F@;��@;dZ@;@:n�@:^5@:M�@:M�@:M�@:M�@:=q@:�@9�#@9�^@9��@9hs@9&�@8Ĝ@8r�@8Q�@8 �@7��@7\)@6��@6�@6�+@6ff@6V@6@5p�@5�@4��@4��@4��@4j@4Z@4I�@4I�@4(�@41@3�m@3�F@3��@3t�@3C�@3o@2��@2�\@2~�@2M�@1�@1��@1��@1��@1hs@1&�@0Ĝ@0bN@0 �@/�;@/��@/l�@/\)@/;d@.��@.�R@.�+@.ff@.E�@.{@-�@-�T@-��@-��@-O�@-?}@,��@,��@,z�@,1@+ƨ@+�F@+��@+33@*�@*��@*�!@*��@*~�@*n�@*M�@*�@)��@)X@)&�@(Ĝ@(�u@(Q�@(1'@'�;@'�w@'|�@'+@'
=@'
=@&��@&�y@&��@&@%��@%�@%O�@$��@$�@$�/@$��@$�j@$�j@$��@$z�@$I�@$1@#�@#C�@#"�@#@"�!@"�\@"~�@"n�@"=q@!��@!��@!��@!x�@!hs@!7L@ ��@ �`@ Ĝ@ �@ r�@ 1'@�;@�;@��@�w@K�@��@ȴ@��@��@ff@5?@{@@�h@�@?}@�/@j@9X@�@1@ƨ@��@�F@S�@o@�@��@��@^5@-@��@��@�7@7L@�@��@�9@r�@Q�@b@�@�;@�;@�@+@��@��@ff@{@@�@@�@O�@V@�/@j@9X@1@�
@ƨ@�F@t�@33@C�@33@@��@��@~�@-@�@�#@��@x�@X@G�@�@�`@�u@Q�@1'@�;@��@�P@��@�P@|�@l�@K�@+@��@�R@��@��@��@�+@�+@v�@v�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  BG�BG�BG�BG�BG�BG�BG�BG�BG�BG�BG�BG�BG�BG�BG�BG�BG�BG�BG�BG�BG�BG�BG�BG�BH�BH�BH�BH�BI�BJ�BJ�BJ�BL�BK�BK�BL�BL�BN�BR�Bt�B�)B��B�=B�hB�FB��B�)B�`B��B	7B
=B	7B�B�B�B�B,B;dB5?B'�B�B�B�B{BuB�B�B�B�BbB	7BB��B�B�HB��B�HB�ZB�ZB�HB�5B�
B��B�jB�B��B��B�7Bv�BcTBT�BJ�BF�B>wB/B �B\B
��B
�yB
�NB
�B
��B
�}B
�FB
�B
��B
�B
t�B
l�B
iyB
hsB
ffB
]/B
Q�B
D�B
.B
�B

=B	��B	�5B	�jB	��B	�bB	m�B	[#B	H�B	=qB	8RB	)�B	�B	DB��B�B�/B��B��BȴBĜB�XB�B�B��B��B��B��B��B�uB�DB�B~�Bv�Bt�Bt�Br�Br�Bp�Bm�BjB`BB`BB]/B[#BZBYBVBR�BQ�BQ�BN�BM�BM�BL�BK�BJ�BH�BG�BE�BB�B?}B?}B@�B?}B?}B>wB=qB=qB<jB:^B8RB5?B33B1'B1'B/B/B.B,B+B+B-B)�B+B)�B)�B)�B(�B(�B)�B(�B(�B(�B(�B'�B+B+B)�B)�B,B+B+B,B,B.B-B0!B0!B0!B1'B0!B0!B2-B33B33B49B6FB8RB<jBA�BG�BI�BF�BL�BJ�BK�BM�BP�BO�BR�BS�BS�BW
BW
BXBZB[#B[#B[#BaHBaHBbNBcTBcTBdZBdZBe`BffBffBhsBo�Bs�Bu�Bv�Bw�By�Bz�Bx�B|�B�B�DB�JB�JB�JB�JB�JB�oB��B��B��B��B��B��B��B��B��B�B�B�B�!B�-B�3B�?B�LB�^B�qB�wB�}BĜB��B��B��B�B�
B�B�B�B�B�5B�NB�ZB�mB�sB�yB�B�B��B��B��B	  B	B	B	B	%B	%B	%B	1B	1B	JB	\B	\B	bB	�B	�B	�B	"�B	%�B	%�B	'�B	+B	-B	/B	0!B	33B	5?B	7LB	;dB	<jB	<jB	>wB	?}B	?}B	A�B	D�B	E�B	I�B	N�B	O�B	Q�B	S�B	W
B	XB	YB	[#B	]/B	_;B	aHB	cTB	dZB	ffB	hsB	iyB	jB	l�B	m�B	p�B	u�B	w�B	y�B	z�B	|�B	� B	�B	�B	�+B	�1B	�1B	�=B	�JB	�VB	�\B	�hB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�3B	�9B	�LB	�XB	�^B	�^B	�jB	�wB	�}B	��B	B	ĜB	ŢB	ŢB	ŢB	ƨB	ƨB	ǮB	ȴB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�B	�B	�#B	�)B	�)B	�/B	�5B	�;B	�BB	�HB	�HB	�NB	�TB	�ZB	�fB	�mB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
1B
1B
	7B
	7B
	7B
	7B

=B

=B

=B
DB
DB
JB
JB
PB
PB
PB
VB
VB
VB
VB
VB
\B
\B
bB
bB
bB
hB
oB
oB
oB
oB
uB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
"�B
#�B
$�B
$�B
$�B
$�B
$�B
%�B
$�B
%�B
%�B
%�B
%�B
&�B
&�B
&�B
(�B
(�B
(�B
+B
+B
+B
+B
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
.B
.B
.B
.B
.B
/B
/B
1'B
1'B
1'B
1'B
2-B
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
5?B
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
6FB
7LB
7LB
7LB
7LB
8RB
8RB
8RB
9XB
9XB
:^B
:^B
;dB
;dB
;dB
;dB
;dB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
=qB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
>wB
>wB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
?}B
@�B
@�B
@�B
@�B
A�B
@�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
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
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
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
N�B
N�B
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
P�B
Q�B
Q�B
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
T�B
T�B
T�B
T�B
VB
VB
VB
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
XB
XB
XB
XB
XB
XB
XB
XB
XB
YB
YB
ZB
YB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
[#B
\)B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
`BB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
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
gmB
gmB
gmB
gmB
hsB
hsB
hsB
iyB
iyB
jB
jB
jB
jB
k�B
k�B
k�B
k�B
l�B
l�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
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
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  BG�BG�BG�BGzBGzBG�BGzBGzBG�BGzBG�BG�BG�BGzBGzBG�BG�BGzBG�BG�BG�BG�BG�BGzBH�BH�BH�BH�BI�BJ�BJ�BJ�BL�BK�BK�BL�BL�BOBBT�Bz*B�pB�vB�jB��B�B҉BܬB�mB�B�BdB~B�B�BB)B/�B=<B7B*B7B�B�B�B?B!bB 'B \B�B�B�BYB;B�AB��B� B��B��B��B�B�;B�B� B��B�OB�zB�EB��ByXBd�BVBK�BG�B@�B1B#nB B
�dB
�B
��B
�B
�dB
��B
��B
�B
�B
��B
utB
l�B
i�B
h�B
h
B
^�B
T{B
G�B
1B
�B
B	��B	�B	�4B	�6B	��B	p�B	^5B	J#B	>�B	;�B	,�B	7B	�B�BB�B�BյBϫB�=B�EB�0B��B�B�XB��B��B��B�?B�B�B��B�Bw�Bu�Bu?BsBs3BrGBp�Bm)BabBabB^�B\]B[�BZ�BV�BS�BSuBR�BN�BM�BN"BMjBLdBK�BI�BIBHBD�B@�B@ BA B@�B@iB?.B>B=�B=�B<PB:*B6�B4�B1�B1�B0!B/�B.�B,qB+�B,�B-�B+B+�B+B+kB+B)�B)�B*0B)DB)�B)�B)�B)yB+�B+kB*B*�B,=B+B+6B,�B,�B.}B-�B0�B0oB0�B2B1B1[B3B3�B3�B4�B6�B8�B=VBB�BIBJ�BHKBM�BJ�BK�BNpBQ�BO�BS@BTaBT,BW�BW�BXyBZ�B[�B[�B\�Ba�Ba�Bb�Bc�Bc�BdtBd�Be�Bf�BgBi�Bp;Bs�Bu�Bv�Bw�Bz*B{dBy�B}�B�B�xB�JB�JB�dB��B�jB��B��B��B��B��B��B��B�B�8B�B�B��B�)B�;B�-B�3B�ZB�fB�^B�qB��B��B�B��B� B�B�B�
B��B�B�B�kBބB�hB�B�mB�XB�yB�B��B��B��B�B	 B	B	B	B	B	�B	%B	KB	KB	JB	BB	BB	�B	�B	�B	�B	"�B	%�B	%�B	(
B	+B	-B	/B	0!B	3B	5?B	7fB	;JB	<6B	<PB	>]B	?cB	?}B	A�B	D�B	E�B	I�B	N�B	O�B	Q�B	S�B	V�B	W�B	X�B	[	B	]/B	_;B	a-B	c:B	dZB	fLB	hXB	i_B	jB	l�B	m�B	p�B	u�B	w�B	y�B	z�B	}B	�4B	�B	�B	�B	��B	�B	�=B	�JB	�<B	�\B	�hB	�oB	�uB	��B	�sB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�)B	�!B	�'B	�3B	�TB	�fB	�>B	�DB	�xB	��B	�]B	�}B	��B	B	āB	ňB	ňB	�mB	ƎB	ƨB	ǔB	ȀB	ȀB	ȚB	ɺB	��B	͹B	͹B	οB	ϫB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	��B	��B	��B	��B	��B	��B	�1B	�#B	�B	�)B	�/B	�B	�;B	�BB	�-B	�HB	�NB	�TB	�B	�fB	�mB	�mB	�B	�yB	�B	�B	�wB	�wB	�wB	�B	�B	�B	��B	�B	�B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B
 �B
B
B
�B
�B
�B
B
�B
�B
B
3B
%B
?B
B
1B
	B
	B
	B
	B

#B

#B

=B
)B
DB
0B
JB
6B
6B
6B
<B
<B
"B
VB
<B
BB
BB
HB
HB
bB
hB
TB
TB
TB
oB
uB
uB
{B
{B
gB
gB
gB
gB
gB
SB
�B
sB
sB
sB
sB
sB
sB
sB
_B
yB
�B
�B
B
�B
kB
kB
�B
�B
�B
qB
�B
xB
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
"�B
#�B
$�B
$�B
$�B
$�B
$�B
%�B
$�B
%�B
%�B
%�B
%�B
&�B
&�B
'B
)B
)B
)B
+B
*�B
*�B
+B
*�B
+�B
+�B
+�B
+�B
+�B
+�B
+�B
,B
,�B
-B
-�B
-�B
-�B
-�B
.B
/ B
/B
1B
1B
1B
1B
1�B
2B
1�B
2-B
3B
2�B
3B
3B
4B
4B
4B
5B
5B
5B
5B
5B
5B
5%B
6+B
6+B
6B
6B
6+B
72B
72B
72B
7LB
8RB
88B
8RB
9XB
9XB
:^B
:DB
;0B
;0B
;JB
;JB
;JB
<jB
<6B
<PB
<6B
<6B
<PB
<PB
=<B
=VB
=VB
=VB
=VB
>]B
>]B
>]B
>]B
>]B
?HB
?cB
?}B
?}B
>]B
>BB
>wB
>]B
>]B
?cB
?cB
?HB
?cB
?HB
@iB
@iB
@iB
@�B
A�B
@iB
AUB
AUB
BuB
BAB
BuB
BuB
BuB
BuB
B�B
C�B
CaB
CaB
CaB
CaB
CGB
C{B
C{B
C{B
C{B
C{B
C{B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
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
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
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
N�B
N�B
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
P�B
Q�B
Q�B
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
T�B
T�B
T�B
T�B
U�B
U�B
U�B
U�B
U�B
U�B
VB
V�B
V�B
V�B
V�B
W�B
W�B
W�B
W�B
W�B
W�B
W�B
W�B
W�B
YB
X�B
ZB
X�B
ZB
ZB
Y�B
ZB
ZB
ZB
ZB
Z�B
[	B
[	B
[	B
[	B
\B
\B
\B
\B
[�B
]B
\�B
\�B
]B
]/B
^B
^B
_B
_!B
_!B
_!B
_!B
_!B
`'B
`'B
`'B
`BB
`BB
a-B
a-B
a-B
bB
b4B
a�B
b4B
b4B
b4B
b4B
b4B
c:B
c:B
c:B
c:B
d@B
d@B
d@B
d&B
d@B
e,B
eFB
eFB
eFB
e,B
e,B
eFB
ffB
fLB
fLB
gRB
gRB
g8B
gRB
gRB
gRB
g8B
g8B
gRB
hXB
hXB
hXB
i_B
i_B
jKB
jeB
jeB
jKB
kQB
k�B
kQB
kQB
l�B
lqB
kQB
lWB
lqB
lqB
lqB
lqB
lqB
mwB
mwB
mwB
mwB
m]B
mwB
nIB
ncB
nIB
ncB
ncB
n�B
n}B
n}B
n}B
ncB
ncB
ncB
ncB
ncB
ncB
ncB
nI111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.42(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         202102030035532021020300355320210203003553202306231727242023062317272420230623172724202102040030532021020400305320210204003053  JA  ARFMdecpA19c                                                                20210129124633  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20210129034703  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20210129034704  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20210129034704  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20210129034704  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20210129034704  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20210129034704  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20210129034704  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20210129034705  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20210129034705                      G�O�G�O�G�O�                JA  ARUP                                                                        20210129035255                      G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20210202153553  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20210202153553  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2019V1                                                       20210203153053  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20230623082724  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20230705041505                      G�O�G�O�G�O�                