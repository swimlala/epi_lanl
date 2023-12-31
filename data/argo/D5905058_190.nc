CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-11-20T00:39:15Z creation;2019-11-20T00:39:19Z conversion to V3.1;2023-06-29T05:50:39Z update;     
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
_FillValue                 �  ]H   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  a4   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �x   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �d   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ޘ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �(   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �(   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �(   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �(   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �    HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �$   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �(Argo profile    3.1 1.2 19500101000000  20191120003915  20230705031506  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0675_190                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @��5�>� 1   @��6����@7f�Ϫ͟�b��e���1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�ffB���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D fD � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!y�D"  D"� D#  D#� D$  D$� D$��D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DXy�DX��DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw�fDx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�<�Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ D�|�D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�ɚD��f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�(�@�A�HA&�HAF�HAf�HA�p�A�p�A�p�A�p�A�p�A�p�A�p�A�p�B�RB	�RB�RB�RB!�RB)�RB1�RB9�RBA�RBI�RBQ�RBY�RBa�RBi�RBq�RBy�RB��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B�B�B���B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)C nCnCnCnCnC
nCnCnCnCnCnCnCnCnCnCnC nC"nC$nC&nC(nC*nC,nC.nC0nC2nC4nC6nC8nC:nC<nC>nC@nCBnCDnCFnCHnCJnCLnCNnCPnCRnCTnCVnCXnCZnC\nC^nC`nCbnCdnCfnChnCjnClnCnnCpnCrnCtnCvnCxnCznC|nC~nC�7
C�7
C�7
C�*=C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�C�C�7
C�7
C�7
C�C�C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�*=C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�C�C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
D !�D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!�D"�D"��D#�D#��D$�D$��D%D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX�DYDY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D��D��D�P�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D�D���D��D�M�DÍ�D���D��D�M�Dč�D���D��D�M�Dō�D���D��D�J�Dƍ�D���D��D�M�DǍ�D���D��D�M�Dȍ�D���D��D�M�Dɍ�D���D��D�M�Dʍ�D���D��D�M�Dˍ�D���D��D�M�D̍�D���D��D�M�D͍�D���D��D�M�D΍�D���D��D�M�Dύ�D���D��D�M�DЍ�D���D��D�M�Dэ�D���D��D�M�Dҍ�D���D��D�M�Dӊ�D���D��D�M�Dԍ�D���D��D�M�DՍ�D���D��D�M�D֍�D���D��D�M�D׍�D���D��D�M�D؍�D���D��D�M�Dٍ�D���D��D�M�Dڍ�D���D��D�M�Dۍ�D���D��D�M�D܍�D���D��D�M�Dݍ�D���D��D�M�Dލ�D���D��D�M�Dߍ�D���D��D�M�D���D���D��D�M�D��D���D��D�M�D��D���D��D�M�D��D���D��D�M�D��D���D��D�M�D��D���D��D�M�D��D���D��D�M�D��D���D��D�M�D��D���D��D�M�D��D���D��D�M�D��D���D��D�M�D��D���D��D�M�D��D���D��D�M�D��D���D��D�M�D��D���D��D�M�D��D���D��D�M�D���D���D��D�M�D��D���D��D�M�D��D���D��D�M�D��D���D��D�M�D��D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D��\D��)1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A���A���A���A���A�A�A�A�A�A��A�K�A�`BAС�A���A��A�(�A�=qA��Aк^A�p�A�?}A�-A��A���A��A�A�ĜA�ȴA϶FAϬAϣ�Aϟ�Aϝ�Aϕ�AύPAσA�|�A�x�A�XA��A���AΛ�A�5?Aˏ\A���A�?}A��mA��A��
A�S�A�z�A�jA��!A�A�JA�XA�&�A�33A�\)A��DA�=qA�5?A�9XA�"�A��mA��`A�&�A��;A�x�A�t�A���A�\)A��A�A�A���A���A��!A���A���A�ĜA��A�ƨA�A�A�XA��A�K�A��HA�C�A�XA�
=A�XA��jA�`BA��hA��A���A��wA��A��#A��^A��A�33A� �A���A�A�S�A�5?A~��A}+Ay��Av�Au/At  AqhsAn�/Ak�TAidZAf��Ac�mAa"�A_��A]�A[dZAY�
AXE�AVjAS��ARQ�AQS�AP^5ANĜAL��AJI�AJ�AI�AI?}AHA�AGVAEAC�TAB�+AA7LA@{A?;dA>�A=S�A;�;A:~�A89XA7hsA6�DA4ffA2��A0��A/oA-�A-|�A,��A+C�A)p�A(r�A'x�A&M�A%/A#�TA#�-A"$�A!�7A!�A 5?A"�AZA�
A�yA�A�A{AK�AC�AK�A�wA�A\)A�Av�AA33A�A�A�uA
=A��AdZAr�A�Ap�A��A�+A�A��A  A
�A
  A	VA�Av�A/A-A �HA  �@�J@���@���@�ȴ@�Ĝ@�bN@�7@��@�R@�=q@���@�@��;@�n�@��@�~�@���@�
=@�ff@�J@��D@�t�@��@��#@�?}@ܓu@�+@ڗ�@ٲ-@�Z@���@�bN@�ȴ@�`B@Л�@Ͼw@�
=@�ff@���@́@�V@��@� �@ʰ!@�O�@�Ĝ@ȓu@�9X@��@��T@�&�@ě�@ģ�@�C�@��y@�ff@�1'@���@�{@�@ċD@î@�K�@�z�@Ĭ@�{@���@�;d@�@�ƨ@��!@�^5@���@��@���@��@��w@��@�n�@�o@��@�v�@�X@��9@�1'@�9X@�1'@�1@�r�@�K�@��!@�M�@��-@��@�Z@�t�@�dZ@�"�@��@�~�@�E�@�-@��@�G�@���@��D@�bN@�A�@�ƨ@�l�@��@�+@���@��7@�x�@�/@��j@��/@���@���@�bN@� �@��@��@�ȴ@���@��\@�V@���@��#@���@�x�@���@��@�Q�@�1'@��m@��@�
=@�C�@�ȴ@�E�@�@���@��@�?}@��`@��D@�A�@� �@��@���@�ƨ@�+@��\@��@��-@���@���@��h@��7@�x�@�p�@�X@�/@��@��9@�1'@��@���@�  @�9X@��@�l�@�K�@�+@�
=@�@�+@�S�@�S�@�"�@���@�n�@���@�x�@���@�@�hs@��@��@��@��u@�Z@��w@�S�@��H@���@�^5@�@���@��@��j@��D@��m@�o@�J@�@�x�@��D@�ƨ@���@��R@��+@��w@�S�@�ȴ@�=q@��#@��^@���@���@�X@�/@�&�@���@�r�@�Z@�b@��w@��P@�dZ@�S�@�C�@�
=@��H@���@�V@�J@���@���@�p�@�G�@���@��@��/@��j@�bN@�(�@�  @��;@��w@�t�@�o@��!@�V@�M�@�5?@�J@���@��@��@��@���@��-@��h@��@��@��@�A�@���@���@��F@��@�K�@��@��H@�ȴ@���@��+@�M�@��#@��7@�?}@�%@���@���@���@�Q�@�9X@�(�@�1@�  @�;@l�@�@~�y@~ȴ@~5?@~@}�T@}@}`B@}?}@}/@|��@|��@|��@|j@|(�@{�m@{C�@z��@y�#@y�7@y7L@x�@xA�@w�@w�;@w�@w|�@wK�@w�@v��@v�y@v�y@vȴ@vff@v$�@u@u`B@u/@t��@t�@st�@r��@r^5@r-@rJ@q�@q�#@q��@qx�@qhs@qX@p�`@pQ�@o�P@o�@nȴ@n��@nv�@n$�@m��@m�@l�j@lI�@l1@kƨ@kƨ@k�F@k�@kS�@ko@k@k@j��@j^5@j-@i��@i��@i��@ihs@i&�@hr�@hQ�@h �@g|�@g�@f�R@f�R@f�+@f{@ep�@e?}@d�/@d�D@d(�@c33@bM�@a�#@aX@`��@`r�@_�@_��@_�@^�+@^5?@^@]p�@]V@\�j@\��@\z�@\9X@[��@[S�@[o@Z�@Z��@ZM�@Y��@Y�7@Y7L@Y%@X��@X�@X  @W\)@V�@Vff@VE�@V$�@U�@U��@U��@U�@Up�@U?}@T�@T9X@Sƨ@SC�@R�!@R=q@Q��@Q��@QX@P��@P1'@O��@O�P@OK�@N��@N��@Nv�@N$�@M��@M@M?}@M�@L�@L�/@L��@L�j@L�@L��@Lj@L9X@L1@Kƨ@Kt�@KdZ@Ko@J�!@Jn�@JM�@I��@I�7@I&�@I�@I%@HĜ@Hr�@HbN@HA�@G��@G|�@GK�@F�@FV@F5?@F{@E�h@Ep�@EO�@E�@D��@D�@D�D@Dj@DI�@D(�@D�@C�m@C��@CdZ@C"�@C@B��@B��@BM�@B=q@BJ@A�@A��@A�@@�`@@�u@@ �@?�w@?�P@?l�@?;d@>�@>��@>$�@=��@=�h@=O�@=?}@=V@<��@<�D@<I�@;�m@;�F@;��@;t�@;o@:��@:��@:n�@:=q@:J@9��@9G�@9%@8�`@8�9@8��@8�@8A�@7�w@7\)@7+@6��@6ȴ@6��@6$�@5�-@5O�@5/@5V@4�j@4Z@4(�@3�m@3ƨ@3�F@3�@3dZ@3S�@333@2�@2~�@2=q@1��@1��@1hs@17L@0�`@0Ĝ@0r�@/�@/�@/|�@/K�@/+@/�@.ȴ@.��@.v�@.E�@.{@-��@-�@-/@,��@,�/@,��@,��@,Z@,1@+ƨ@+�@+S�@*�@*��@*^5@*-@)��@)�#@)��@)x�@)7L@(��@(�9@(r�@(A�@(b@'�@'|�@'l�@'
=@&ȴ@&�+@&v�@&ff@&V@&E�@&$�@%�T@%��@%�h@%�@%O�@%/@$�@$�@$j@$1@#�
@#t�@#C�@"�@"��@"�!@"��@"~�@"M�@"�@!��@!�^@!hs@!%@ ��@ �`@ ��@ �u@ A�@ b@�w@�P@\)@�@�@��@��@ff@E�@5?@@p�@?}@�@��@�@�D@Z@(�@��@ƨ@�F@�@C�@33@�@�!@�\@~�@=q@�^@��@X@&�@�`@�9@��@�u@A�@ �@  @�@��@|�@\)@�@��@�@�R@��@��@�+@V@E�@�T@�@`B@O�@�@�@�j@�j@�@j@��@��@��@�@S�@33@"�@�H@��@~�@M�@-@J@��@�#@�7@7L@��@Ĝ@��@bN@ �@b@�;@�@�P@�@ȴ@�+@ff@V@E�@@@�h@p�@O�@V@�/@�j@�@j@�@1@1@1@�m@ƨ@��@33@
��@
�!@
~�@
M�@
-@	�^@	��@	��@	��@	x�@	X@	7L@	�@�`@Ĝ@�9@��@��@�u@r�@Q�@1'@b@b@b1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A���A���A���A���A�A�A�A�A�A��A�K�A�`BAС�A���A��A�(�A�=qA��Aк^A�p�A�?}A�-A��A���A��A�A�ĜA�ȴA϶FAϬAϣ�Aϟ�Aϝ�Aϕ�AύPAσA�|�A�x�A�XA��A���AΛ�A�5?Aˏ\A���A�?}A��mA��A��
A�S�A�z�A�jA��!A�A�JA�XA�&�A�33A�\)A��DA�=qA�5?A�9XA�"�A��mA��`A�&�A��;A�x�A�t�A���A�\)A��A�A�A���A���A��!A���A���A�ĜA��A�ƨA�A�A�XA��A�K�A��HA�C�A�XA�
=A�XA��jA�`BA��hA��A���A��wA��A��#A��^A��A�33A� �A���A�A�S�A�5?A~��A}+Ay��Av�Au/At  AqhsAn�/Ak�TAidZAf��Ac�mAa"�A_��A]�A[dZAY�
AXE�AVjAS��ARQ�AQS�AP^5ANĜAL��AJI�AJ�AI�AI?}AHA�AGVAEAC�TAB�+AA7LA@{A?;dA>�A=S�A;�;A:~�A89XA7hsA6�DA4ffA2��A0��A/oA-�A-|�A,��A+C�A)p�A(r�A'x�A&M�A%/A#�TA#�-A"$�A!�7A!�A 5?A"�AZA�
A�yA�A�A{AK�AC�AK�A�wA�A\)A�Av�AA33A�A�A�uA
=A��AdZAr�A�Ap�A��A�+A�A��A  A
�A
  A	VA�Av�A/A-A �HA  �@�J@���@���@�ȴ@�Ĝ@�bN@�7@��@�R@�=q@���@�@��;@�n�@��@�~�@���@�
=@�ff@�J@��D@�t�@��@��#@�?}@ܓu@�+@ڗ�@ٲ-@�Z@���@�bN@�ȴ@�`B@Л�@Ͼw@�
=@�ff@���@́@�V@��@� �@ʰ!@�O�@�Ĝ@ȓu@�9X@��@��T@�&�@ě�@ģ�@�C�@��y@�ff@�1'@���@�{@�@ċD@î@�K�@�z�@Ĭ@�{@���@�;d@�@�ƨ@��!@�^5@���@��@���@��@��w@��@�n�@�o@��@�v�@�X@��9@�1'@�9X@�1'@�1@�r�@�K�@��!@�M�@��-@��@�Z@�t�@�dZ@�"�@��@�~�@�E�@�-@��@�G�@���@��D@�bN@�A�@�ƨ@�l�@��@�+@���@��7@�x�@�/@��j@��/@���@���@�bN@� �@��@��@�ȴ@���@��\@�V@���@��#@���@�x�@���@��@�Q�@�1'@��m@��@�
=@�C�@�ȴ@�E�@�@���@��@�?}@��`@��D@�A�@� �@��@���@�ƨ@�+@��\@��@��-@���@���@��h@��7@�x�@�p�@�X@�/@��@��9@�1'@��@���@�  @�9X@��@�l�@�K�@�+@�
=@�@�+@�S�@�S�@�"�@���@�n�@���@�x�@���@�@�hs@��@��@��@��u@�Z@��w@�S�@��H@���@�^5@�@���@��@��j@��D@��m@�o@�J@�@�x�@��D@�ƨ@���@��R@��+@��w@�S�@�ȴ@�=q@��#@��^@���@���@�X@�/@�&�@���@�r�@�Z@�b@��w@��P@�dZ@�S�@�C�@�
=@��H@���@�V@�J@���@���@�p�@�G�@���@��@��/@��j@�bN@�(�@�  @��;@��w@�t�@�o@��!@�V@�M�@�5?@�J@���@��@��@��@���@��-@��h@��@��@��@�A�@���@���@��F@��@�K�@��@��H@�ȴ@���@��+@�M�@��#@��7@�?}@�%@���@���@���@�Q�@�9X@�(�@�1@�  @�;@l�@�@~�y@~ȴ@~5?@~@}�T@}@}`B@}?}@}/@|��@|��@|��@|j@|(�@{�m@{C�@z��@y�#@y�7@y7L@x�@xA�@w�@w�;@w�@w|�@wK�@w�@v��@v�y@v�y@vȴ@vff@v$�@u@u`B@u/@t��@t�@st�@r��@r^5@r-@rJ@q�@q�#@q��@qx�@qhs@qX@p�`@pQ�@o�P@o�@nȴ@n��@nv�@n$�@m��@m�@l�j@lI�@l1@kƨ@kƨ@k�F@k�@kS�@ko@k@k@j��@j^5@j-@i��@i��@i��@ihs@i&�@hr�@hQ�@h �@g|�@g�@f�R@f�R@f�+@f{@ep�@e?}@d�/@d�D@d(�@c33@bM�@a�#@aX@`��@`r�@_�@_��@_�@^�+@^5?@^@]p�@]V@\�j@\��@\z�@\9X@[��@[S�@[o@Z�@Z��@ZM�@Y��@Y�7@Y7L@Y%@X��@X�@X  @W\)@V�@Vff@VE�@V$�@U�@U��@U��@U�@Up�@U?}@T�@T9X@Sƨ@SC�@R�!@R=q@Q��@Q��@QX@P��@P1'@O��@O�P@OK�@N��@N��@Nv�@N$�@M��@M@M?}@M�@L�@L�/@L��@L�j@L�@L��@Lj@L9X@L1@Kƨ@Kt�@KdZ@Ko@J�!@Jn�@JM�@I��@I�7@I&�@I�@I%@HĜ@Hr�@HbN@HA�@G��@G|�@GK�@F�@FV@F5?@F{@E�h@Ep�@EO�@E�@D��@D�@D�D@Dj@DI�@D(�@D�@C�m@C��@CdZ@C"�@C@B��@B��@BM�@B=q@BJ@A�@A��@A�@@�`@@�u@@ �@?�w@?�P@?l�@?;d@>�@>��@>$�@=��@=�h@=O�@=?}@=V@<��@<�D@<I�@;�m@;�F@;��@;t�@;o@:��@:��@:n�@:=q@:J@9��@9G�@9%@8�`@8�9@8��@8�@8A�@7�w@7\)@7+@6��@6ȴ@6��@6$�@5�-@5O�@5/@5V@4�j@4Z@4(�@3�m@3ƨ@3�F@3�@3dZ@3S�@333@2�@2~�@2=q@1��@1��@1hs@17L@0�`@0Ĝ@0r�@/�@/�@/|�@/K�@/+@/�@.ȴ@.��@.v�@.E�@.{@-��@-�@-/@,��@,�/@,��@,��@,Z@,1@+ƨ@+�@+S�@*�@*��@*^5@*-@)��@)�#@)��@)x�@)7L@(��@(�9@(r�@(A�@(b@'�@'|�@'l�@'
=@&ȴ@&�+@&v�@&ff@&V@&E�@&$�@%�T@%��@%�h@%�@%O�@%/@$�@$�@$j@$1@#�
@#t�@#C�@"�@"��@"�!@"��@"~�@"M�@"�@!��@!�^@!hs@!%@ ��@ �`@ ��@ �u@ A�@ b@�w@�P@\)@�@�@��@��@ff@E�@5?@@p�@?}@�@��@�@�D@Z@(�@��@ƨ@�F@�@C�@33@�@�!@�\@~�@=q@�^@��@X@&�@�`@�9@��@�u@A�@ �@  @�@��@|�@\)@�@��@�@�R@��@��@�+@V@E�@�T@�@`B@O�@�@�@�j@�j@�@j@��@��@��@�@S�@33@"�@�H@��@~�@M�@-@J@��@�#@�7@7L@��@Ĝ@��@bN@ �@b@�;@�@�P@�@ȴ@�+@ff@V@E�@@@�h@p�@O�@V@�/@�j@�@j@�@1@1@1@�m@ƨ@��@33@
��@
�!@
~�@
M�@
-@	�^@	��@	��@	��@	x�@	X@	7L@	�@�`@Ĝ@�9@��@��@�u@r�@Q�@1'@b@b@b1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
��B
��B
�B
�B
��B
�B
��B
��B
�B
�3B
��B
�)BB)�B;dBZBn�B~�B� B�B�B�DB�\B�oB�hB�oB��B��B��B��B��B��B��B�B�B�B�B�B�3B�RB�^B�^B��B��B��B�9B��B�B�B��B%BuB{BJB+B�B!�BK�BW
B\)BbNBiyBdZB`BB`BB\)BZBVBT�BN�BM�BK�BP�BR�BQ�BM�BA�B2-B%�B�B\B
=BB��B�
BɺBŢB�^B�B�uB�%B� BhsBT�B>wB)�BuBB
�B
ɺB
�?B
��B
��B
�+B
t�B
VB
;dB
(�B
�B
B	�sB	�B	��B	�dB	��B	�hB	}�B	k�B	ZB	D�B	9XB	/B	�B	uB	1B��B�B�TB�)B�
B��BƨB�FB�9B�'B�B�B��B��B��B�VB�%B�B}�By�Bu�Bp�Bm�Be`B_;B[#BW
BQ�BN�BI�BE�BC�BB�BE�BA�BA�B?}B>wB?}B=qB=qB>wB@�BH�BL�BO�BM�BJ�BF�BD�BF�BJ�BL�BS�B[#BYB]/BcTBdZBe`BcTB\)B_;BcTBdZB_;B\)BZB[#BXBW
BT�BYB\)B[#BXBT�BR�BR�BN�BA�B7LB1'B0!B-B+B)�B'�B%�B#�B!�B'�B%�B'�B'�B+B/B0!B1'B49B5?B5?B9XB9XB:^B<jB9XB8RB:^B=qB@�B@�B@�BA�BB�BA�B?}B?}B=qB>wBC�BC�BD�BE�BF�BF�BF�BJ�BI�BN�BQ�BR�BR�BW
BT�BS�BW
BbNBdZBhsBiyBv�B�B�JB�hB��B��B��B��B�^B�-B�B��B��B��B��B��B��B��B��B��B��B�^BÖB��B��B�B��B�B�/B�;B�;B�ZB��B��B��B��B	B	B	B		7B	
=B	PB	bB	{B	�B	�B	�B	�B	"�B	&�B	)�B	-B	0!B	.B	0!B	2-B	9XB	:^B	>wB	C�B	F�B	J�B	M�B	R�B	T�B	XB	[#B	`BB	aHB	bNB	bNB	cTB	cTB	cTB	cTB	dZB	gmB	jB	l�B	l�B	o�B	p�B	r�B	w�B	z�B	|�B	}�B	~�B	~�B	~�B	� B	�B	�B	�B	�B	�B	�B	�1B	�JB	�PB	�VB	�VB	�VB	�\B	�\B	�bB	�bB	�bB	�hB	�hB	�hB	�hB	�oB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�'B	�3B	�3B	�3B	�3B	�3B	�3B	�3B	�-B	�'B	�!B	�B	�B	�B	��B	�B	�B	��B	��B	�B	��B	��B	��B	�B	�B	�!B	�'B	�-B	�9B	�?B	�?B	�LB	�LB	�LB	�XB	�dB	�dB	�jB	�jB	�jB	�wB	�wB	�}B	��B	B	ÖB	ŢB	ƨB	ƨB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�#B	�#B	�)B	�)B	�)B	�)B	�)B	�/B	�5B	�;B	�;B	�HB	�NB	�TB	�ZB	�ZB	�`B	�`B	�fB	�mB	�mB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
%B
+B
1B
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
PB
PB
VB
VB
\B
\B
VB
\B
\B
\B
bB
bB
bB
bB
bB
hB
hB
oB
uB
uB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
#�B
#�B
$�B
$�B
%�B
%�B
%�B
&�B
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
-B
-B
-B
-B
-B
-B
-B
-B
.B
.B
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
2-B
2-B
2-B
2-B
33B
33B
33B
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
6FB
5?B
6FB
6FB
7LB
7LB
7LB
8RB
8RB
8RB
8RB
8RB
8RB
9XB
9XB
9XB
:^B
:^B
:^B
:^B
;dB
;dB
<jB
<jB
<jB
<jB
=qB
=qB
=qB
>wB
=qB
>wB
>wB
?}B
?}B
?}B
?}B
?}B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
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
K�B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
M�B
L�B
M�B
M�B
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
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
T�B
T�B
T�B
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
W
B
XB
XB
XB
XB
XB
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
ZB
ZB
[#B
[#B
[#B
[#B
[#B
[#B
[#B
\)B
\)B
\)B
]/B
\)B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
`BB
`BB
aHB
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
dZB
e`B
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
ffB
ffB
gmB
gmB
gmB
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
jB
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
p�B
p�B
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
r�B
r�B
r�B
r�B
r�B
r�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
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
�VB
��B�B)�B;BY�Bo BcB�iB�UB�9B�^B��B��B�hB�TB�yB��B��B��B��B��B��B�B�B�B�B�IB��B��B��B��BбB�B��B��B�.B#�B�B�BKB�B9B�B	B�B$&BM�BY1B_pBf2Bk�Bf�Bb�BbB]~BZ�BW$BW
BQ BP.BL�BR:BT{BS�BP�BC�B49B'�B�BHB�BGB�B�yB��B�+B��B��B��B��B��BjeBW$B@OB,WB�BB
�B
��B
��B
�qB
�jB
��B
xB
X�B
=�B
+QB
eB
B	�0B	��B	� B	�wB	�XB	��B	� B	n�B	\�B	FtB	;�B	1�B	!�B	�B	
�B��B�UB�BݘB��B�\BȴB��B��B��B�OB�qB��B��B�+B��B�zB�BHBz�Bw�Br|BpBf�B`�B]�BYKBTBP�BKBFtBD�BDgBG�BB�BB�B@�B?�B@�B>B>�B?.BAUBI�BNBP�BN�BK�BG�BE�BG�BK^BL�BTFB\�BYeB]dBc�Bd�BfBc�B\B_�Bd@BfB`�B\�B[=B[�BXEBW�BUgBY�B]~B\]BYeBVSBT�BU�BQ�BC-B8�B2�B1AB.}B,�B+�B)*B'B$tB#TB(�B&�B($B(>B+�B/�B1'B2aB5tB6+B6FB9�B9�B;0B<�B9�B8�B:�B=�BA;B@�BA BBuBC�BB�B@iB@4B=�B>�BC�BC�BD�BE�BF�BF�BG_BK�BJrBN�BQ�BS&BS�BW�BUgBT,BW$Bb�BdtBh>BhXBvB��B�~B�B��B�yB�)B�$B��B��B�/B�B��B�mB�B�NB�`B��B�\B��B��B��B�-BοB�TBևB�FB�+B��B�B�!B�@B�2B�B�B�<B	[B	{B	mB		B	
XB	jB	}B	{B	�B	�B	�B	�B	"�B	&�B	)�B	-CB	0;B	./B	0B	2|B	9�B	:DB	>�B	C�B	FtB	J�B	M�B	R�B	UB	XEB	[WB	`'B	aB	b4B	bNB	cTB	cTB	c:B	cTB	dtB	g�B	jB	l�B	l�B	o�B	p�B	r�B	xB	z�B	|�B	}�B	~�B	~�B	B	� B	��B	��B	��B	�B	�3B	�gB	�fB	�~B	�PB	�<B	�<B	�<B	�BB	�BB	�HB	�.B	�bB	�hB	��B	��B	�NB	�TB	�:B	�@B	��B	��B	�MB	�MB	�gB	�SB	�+B	�kB	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	� B	�5B	�OB	�AB	�MB	�3B	�3B	�hB	�3B	�hB	�3B	�GB	�vB	��B	��B	�"B	�6B	�B	�qB	�QB	��B	�_B	�WB	�B	�
B	�B	�B	� B	�B	��B	�B	�B	�%B	�ZB	�2B	�2B	�fB	�>B	�JB	�JB	�PB	�PB	�PB	�]B	�]B	��B	��B	B	ÖB	ňB	ƎB	ƨB	ǔB	˒B	��B	��B	��B	��B	��B	��B	��B	�B	�+B	�B	�B	�	B	�	B	�B	��B	��B	�B	��B	�B	�5B	�VB	�VB	�HB	�NB	�TB	�@B	�&B	�`B	�FB	�fB	�RB	�RB	�XB	�sB	�sB	�B	�B	�B	�B	�wB	�wB	�wB	�}B	�B	�B	�B	�oB	��B	�B	�B	�vB	�vB	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
 �B
B
 B
-B
-B
�B
B
B
�B
�B
�B
�B
�B
B
%B
+B
1B
	B
	B

#B

#B

=B
DB
)B
JB
JB
B
6B
B
6B
6B
"B
"B
(B
BB
<B
(B
BB
BB
HB
HB
HB
.B
HB
4B
NB
oB
[B
[B
@B
aB
{B
gB
mB
�B
�B
sB
�B
�B
�B
�B
�B
yB
�B
eB
�B
�B
B
B
�B
�B
kB
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
#�B
#�B
$�B
$�B
%�B
%�B
%�B
&�B
&�B
'B
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
*�B
*�B
*�B
*�B
+�B
+�B
+�B
+�B
+�B
+�B
+�B
,�B
,�B
,�B
,�B
,�B
,�B
,�B
-B
-�B
-�B
-�B
-�B
-�B
.�B
/ B
/B
.�B
0B
0!B
0!B
1B
0�B
1'B
2B
1�B
2B
2B
2B
2B
3B
2�B
3B
3B
3B
3B
3B
3B
4B
4B
4B
4B
5B
5%B
6+B
5?B
6FB
6B
72B
7LB
72B
88B
88B
88B
8RB
88B
8RB
9>B
9>B
9>B
:*B
:DB
:DB
:DB
;JB
;JB
<PB
<PB
<PB
<jB
=VB
=VB
=VB
>]B
=VB
>wB
>]B
?cB
?cB
?cB
?cB
?HB
@OB
@�B
AoB
AoB
AoB
AoB
AoB
B�B
B�B
BuB
C{B
C{B
C{B
C�B
D�B
D�B
DgB
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
G�B
G�B
GzB
G�B
G�B
H�B
H�B
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
K�B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
M�B
L�B
M�B
M�B
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
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
T�B
T�B
T�B
U�B
U�B
U�B
U�B
U�B
U�B
V�B
V�B
V�B
V�B
V�B
W�B
W�B
W�B
W�B
W�B
X�B
X�B
X�B
ZB
ZB
ZB
ZB
Y�B
ZB
ZB
ZB
ZB
[	B
[	B
[	B
Z�B
[	B
[	B
Z�B
\B
\B
[�B
\�B
\B
]B
]B
]B
^B
^B
^B
^B
^5B
_!B
_!B
_!B
_!B
`'B
`B
`'B
`B
`'B
`'B
`�B
a-B
a-B
a-B
a-B
b4B
b4B
b4B
bB
bB
b4B
bB
bB
b4B
c:B
c:B
c:B
c:B
d@B
d@B
d&B
d@B
d@B
d@B
eFB
e,B
eFB
eFB
eFB
e,B
eFB
eFB
fLB
fLB
fLB
fLB
f2B
gRB
gRB
gRB
hXB
hXB
hXB
hXB
i_B
iDB
i_B
i_B
i_B
iyB
jKB
jeB
jeB
jKB
jeB
kkB
kkB
kkB
kkB
lqB
lqB
lqB
lqB
lWB
lqB
mwB
m]B
m]B
m]B
mwB
mwB
m]B
mwB
mwB
mwB
n}B
n}B
n}B
oiB
oOB
oiB
poB
poB
p�B
p�B
qvB
q�B
q�B
qvB
qvB
qvB
qvB
q�B
r|B
r�B
r|B
r|B
raB
r|1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<m�1<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.43(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201911250039122019112500391220191125003912202306231719052023062317190520230623171905201911260028172019112600281720191126002817  JA  ARFMdecpA19c                                                                20191120093849  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20191120003915  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20191120003917  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20191120003917  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20191120003918  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20191120003918  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20191120003918  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20191120003918  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20191120003919  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20191120003919                      G�O�G�O�G�O�                JA  ARUP                                                                        20191120005554                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20191120153718  CV  JULD            G�O�G�O�F�i�                JM  ARCAJMQC2.0                                                                 20191124153912  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20191124153912  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20191125152817  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20230623081905  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20230705031506                      G�O�G�O�G�O�                