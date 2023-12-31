CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-12-02T18:37:28Z creation;2019-12-02T18:37:34Z conversion to V3.1;2023-06-29T05:50:34Z update;     
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
resolution        =���     �  Ml   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  pX   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t<   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �t   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �|   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �    SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ݐ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �    HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �`   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �p   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �t   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20191202183728  20230705031507  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0675_193                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @��i��� 1   @��jl��@7q-w1���b��X�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C	�fC�fC  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D!��D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� D`��Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt�fDu  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�C3Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ D˃3D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�3D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�FfD�i�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��\@�A�HA&�HAF�HAf�HA�p�A�p�A�p�A�p�A�p�A�p�A�p�A�p�B�RB	�RB�RB�RB!�RB)�RB1�RB9�RBA�RBI�RBQ�RBY�RBa�RBi�RBq�RBy�RB��)B��)B��)B��)B��)B��)B��)B���B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)C nCnCnCnCnC
T{CT{CnCnCnCnCnCnCnCnCnC nC"nC$nC&nC(nC*nC,nC.nC0nC2nC4nC6nC8nC:nC<nC>nC@nCBnCDnCFnCHnCJnCLnCNnCPnCRnCTnCVnCXnCZnC\nC^nC`nCbnCdnCfnChnCjnClnCnnCpnCrnCtnCvnCxnCznC|nC~nC�7
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
D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��DaDa��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D��D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�M�D�D���D��D�M�DÍ�D���D��D�M�Dč�D���D��D�M�Dō�D���D��D�M�Dƍ�D���D��D�P�DǍ�D���D��D�M�Dȍ�D���D��D�M�Dɍ�D���D��D�M�Dʍ�D���D��D�M�Dː�D���D��D�M�D̍�D���D��D�M�D͍�D���D��D�M�D΍�D���D��D�M�Dύ�D���D��D�M�DЍ�D���D��D�M�Dэ�D���D��D�M�Dҍ�D���D��D�M�DӍ�D���D��D�M�Dԍ�D���D��D�M�DՍ�D���D��D�M�D֍�D���D��D�M�D׍�D���D��D�M�D؍�D���D��D�M�Dٍ�D���D��D�M�Dڍ�D���D��D�M�Dۍ�D���D��D�M�D܍�D���D��D�M�Dݍ�D���D��D�M�Dލ�D���D��D�M�Dߍ�D���D��D�M�D���D���D��D�M�D��D���D��D�M�D��D���D��D�M�D��D���D��D�M�D��D���D��D�M�D��D���D��D�M�D��D���D��D�M�D��D���D��D�M�D��D���D��D�M�D��D���D��D�M�D��D���D��D�M�D��D���D��D�M�D��D���D��D�M�D��D���D��D�M�D��D���D��D�M�D��D���D��D�M�D���D���D��D�M�D��D���D��D�M�D��D���D��D�M�D��D���D��D�M�D��D���D��D�M�D���D���D��D�M�D���D���D��D�M�D���D���D��D�T)D�w\111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   Aʲ-A�ƨA���A���A���A���A���A��A��#A��#A��#A��#A��/A��;A��HA��TA��TA��HA��TA��TA��;A���A���A���A���A���A���A���A��#A��#A���A�ƨA�ĜA�ĜAʾwAʡ�A�I�Aɩ�A�1'A��;A���A��A��!A�&�A��A���A�ZA�|�A�{A���A�jA�\)A��RA�ffA���A��HA�n�A��FA�K�A��uA�E�A��
A�VA�=qA���A�dZA�$�A���A�&�A�{A��DA�1'A���A�Q�A��yA�^5A�{A���A�^5A��7A��`A�5?A���A�1A�$�A�1A��A�I�A��A�C�A��`A��PA��\A�K�A���A�ƨA��A��-A���A��;A�C�A�r�A�p�A��
A�l�A�A�A��A��;A�x�A�C�A���A���A�\)A��Az��Ax��AtQ�Ap^5An��Am��AkS�Ah9XAd�Ab��Aa/A_33A]�A\�AW��AT�9AR�yAQ?}APjAO�;AOt�AN��AM��AK+AI�FAF��AE��AE�hAE�AD=qACS�ABAA"�A>�/A:�yA7�
A6�A3��A2�\A1t�A/l�A-�TA,5?A+A*=qA)G�A((�A'33A&(�A%S�A$�jA${A#�A"��A!��A ��A 5?A �A��A��A�AbAx�A��Ap�AQ�A�A�Ax�A~�A��A��Az�A|�A+A�HA\)A�7A��AAS�A
�RA	�^A��A&�AjAI�AC�A��A(�AAl�A
=A��AVAƨA
=A Z@��F@��@�{@�Z@�%@�I�@���@��#@�@�M�@�{@��`@�ƨ@�@�?}@�w@�j@��;@���@��y@��D@���@��T@�X@ܼj@ە�@ڟ�@�v�@ٙ�@�Ĝ@ם�@���@�I�@��
@�b@��@ӶF@�C�@ҏ\@��@���@�Z@Ϯ@�"�@�o@��y@���@�\)@ϥ�@�
=@�=q@��H@�|�@�t�@ϝ�@�@ͩ�@�(�@ʇ+@�o@�ƨ@ˍP@�C�@ʧ�@��T@ȃ@ƸR@�Ĝ@���@�hs@���@�5?@Ł@ř�@��@�X@Ĵ9@ģ�@�/@ũ�@���@�O�@�A�@��@��@�@��j@�ƨ@�dZ@�t�@���@��F@�Ĝ@�`B@��@���@�A�@��m@�S�@�@��!@�5?@�J@�`B@��@�b@��
@���@�~�@�E�@��@��y@��+@�@�r�@�|�@�J@�&�@�hs@��#@�p�@�x�@��7@��7@�?}@��u@�\)@�+@�
=@�5?@�E�@�7L@��m@��m@��@���@�dZ@���@�~�@�=q@��@��-@��@��@���@��@��w@��F@��;@�ƨ@���@�;d@�
=@�ȴ@�V@���@���@�@��-@�hs@���@��9@�A�@��m@��@�33@�o@��@��R@���@��\@�~�@�^5@�5?@�J@���@���@��7@�x�@�?}@���@�Z@�Q�@��@��P@��R@� �@���@���@���@��@���@�ȴ@��@���@��T@�hs@���@���@�1@�K�@�V@�9X@� �@���@�~�@���@���@�(�@�  @��
@�l�@�A�@��@�/@��@�ƨ@�K�@�v�@�@��7@��@��/@���@�Q�@��@��\@�M�@��T@���@��7@��@�`B@�/@�%@��9@��@�Q�@� �@�b@�1@��w@��@�;d@�@���@���@��\@�^5@�$�@��T@��7@�/@��@��/@��9@�bN@�1@��m@��w@�|�@�S�@��@��@��H@���@��\@�M�@�{@��-@���@��7@�X@�X@�/@�%@�Z@�9X@��@���@��;@���@�\)@�K�@�+@��@��@��!@���@�~�@�ff@�J@���@��h@��7@�p�@�/@��@���@��j@���@�A�@�@\)@+@~�y@~v�@~$�@}�-@}`B@}V@|�j@|j@{�m@{��@{�@{C�@z�\@z^5@z^5@z-@y��@yG�@y&�@y�@x�`@x�u@xA�@w�@w|�@w
=@vȴ@vv�@u��@u�@up�@u`B@uO�@u�@uV@t�D@s�@s"�@r~�@q��@q�7@q7L@p�`@pbN@o��@o�@o;d@nff@n@m�@l�@l(�@k�F@k��@k�@kS�@k@j�\@j�@i�@i�#@i�^@i�7@h��@h��@hbN@h  @g�@g|�@gK�@f��@f5?@f{@e��@e`B@e/@eV@d��@dj@d9X@cƨ@c"�@c@b�H@b�!@a�@a��@ahs@a�@`�`@`��@` �@_l�@_�@^��@^5?@]�T@]@]��@]?}@\��@\��@\�j@\�@\��@\I�@\1@[��@[t�@[o@Z��@Z�!@Z�!@Z��@Z��@Zn�@Y�#@Yhs@YG�@X��@X�u@Xb@W�w@W|�@WK�@V��@VE�@U��@UO�@T�/@T��@TZ@T1@S�m@S�F@St�@SC�@R�H@R�!@R�\@R~�@R~�@R~�@R^5@RM�@Q�@Q��@QX@Q%@P�`@P��@PbN@PQ�@P �@P  @O��@O\)@N�@N��@NV@N{@M��@Mp�@MO�@M?}@L��@L��@LI�@K��@K�
@Kƨ@K��@KdZ@KC�@K@J��@J��@JM�@JM�@J=q@I�^@IX@I&�@H�9@HbN@HQ�@HA�@H �@G�;@Gl�@Fȴ@F��@F��@F��@Fv�@F5?@E�@E��@E?}@EV@D�/@D��@D��@DZ@C�F@CC�@C"�@B�@B��@B�\@BM�@A��@Ahs@A�@@��@@�@@bN@?�;@?\)@?
=@>ȴ@>V@>@=�-@=�h@<�@<�@<�@;�F@;��@;�@;t�@;dZ@;S�@;33@;o@;o@;@:��@:=q@9�@9��@9&�@8��@8�u@8r�@81'@8b@7�@7�;@7�@7+@6�@6ȴ@6ff@6{@5@5�h@5O�@5V@4�j@4z�@4Z@3��@3��@3o@3o@3@2�H@2�!@2^5@1�@1��@1&�@0�9@0A�@/�@/�;@/�@/|�@/;d@/�@/
=@.�y@.��@.V@.{@.@-�-@-�@-�@,�@,j@,Z@,I�@,Z@,I�@,(�@+�m@+�@+33@*�H@*n�@*-@)��@)�#@)��@)G�@)%@(��@(b@'�;@'�P@'�@&ȴ@&�+@&V@&5?@%�T@%�@%O�@%/@%V@$�/@$��@$j@$9X@$�@#��@#�m@#�
@#�F@#�F@#��@#t�@#dZ@#�@#�@#dZ@#"�@"�@"��@"�\@"~�@"^5@"�@"�@!�^@!x�@!hs@!7L@ ��@ �`@ Ĝ@ Ĝ@ Ĝ@ �9@ ��@ r�@ Q�@ 1'@  �@�;@��@��@�@|�@\)@�@��@�R@��@v�@ff@ff@5?@{@{@{@@��@/@V@��@�@��@�@�@�@��@Z@�@�m@��@dZ@dZ@S�@"�@@�@��@�!@n�@=q@=q@=q@J@��@��@�7@x�@&�@��@��@Ĝ@��@�@ �@  @��@|�@K�@�@�@ȴ@��@E�@�T@��@�-@��@�h@O�@�@��@�j@�@��@��@j@I�@(�@��@��@�m@�
@�F@��@��@S�@33@�@�!@^5@J@��@�#@��@X@7L@�@�`@�9@�u@�@r�@1'@  @�w@l�@\)@;d@
=@�R@��@ff111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   Aʲ-A�ƨA���A���A���A���A���A��A��#A��#A��#A��#A��/A��;A��HA��TA��TA��HA��TA��TA��;A���A���A���A���A���A���A���A��#A��#A���A�ƨA�ĜA�ĜAʾwAʡ�A�I�Aɩ�A�1'A��;A���A��A��!A�&�A��A���A�ZA�|�A�{A���A�jA�\)A��RA�ffA���A��HA�n�A��FA�K�A��uA�E�A��
A�VA�=qA���A�dZA�$�A���A�&�A�{A��DA�1'A���A�Q�A��yA�^5A�{A���A�^5A��7A��`A�5?A���A�1A�$�A�1A��A�I�A��A�C�A��`A��PA��\A�K�A���A�ƨA��A��-A���A��;A�C�A�r�A�p�A��
A�l�A�A�A��A��;A�x�A�C�A���A���A�\)A��Az��Ax��AtQ�Ap^5An��Am��AkS�Ah9XAd�Ab��Aa/A_33A]�A\�AW��AT�9AR�yAQ?}APjAO�;AOt�AN��AM��AK+AI�FAF��AE��AE�hAE�AD=qACS�ABAA"�A>�/A:�yA7�
A6�A3��A2�\A1t�A/l�A-�TA,5?A+A*=qA)G�A((�A'33A&(�A%S�A$�jA${A#�A"��A!��A ��A 5?A �A��A��A�AbAx�A��Ap�AQ�A�A�Ax�A~�A��A��Az�A|�A+A�HA\)A�7A��AAS�A
�RA	�^A��A&�AjAI�AC�A��A(�AAl�A
=A��AVAƨA
=A Z@��F@��@�{@�Z@�%@�I�@���@��#@�@�M�@�{@��`@�ƨ@�@�?}@�w@�j@��;@���@��y@��D@���@��T@�X@ܼj@ە�@ڟ�@�v�@ٙ�@�Ĝ@ם�@���@�I�@��
@�b@��@ӶF@�C�@ҏ\@��@���@�Z@Ϯ@�"�@�o@��y@���@�\)@ϥ�@�
=@�=q@��H@�|�@�t�@ϝ�@�@ͩ�@�(�@ʇ+@�o@�ƨ@ˍP@�C�@ʧ�@��T@ȃ@ƸR@�Ĝ@���@�hs@���@�5?@Ł@ř�@��@�X@Ĵ9@ģ�@�/@ũ�@���@�O�@�A�@��@��@�@��j@�ƨ@�dZ@�t�@���@��F@�Ĝ@�`B@��@���@�A�@��m@�S�@�@��!@�5?@�J@�`B@��@�b@��
@���@�~�@�E�@��@��y@��+@�@�r�@�|�@�J@�&�@�hs@��#@�p�@�x�@��7@��7@�?}@��u@�\)@�+@�
=@�5?@�E�@�7L@��m@��m@��@���@�dZ@���@�~�@�=q@��@��-@��@��@���@��@��w@��F@��;@�ƨ@���@�;d@�
=@�ȴ@�V@���@���@�@��-@�hs@���@��9@�A�@��m@��@�33@�o@��@��R@���@��\@�~�@�^5@�5?@�J@���@���@��7@�x�@�?}@���@�Z@�Q�@��@��P@��R@� �@���@���@���@��@���@�ȴ@��@���@��T@�hs@���@���@�1@�K�@�V@�9X@� �@���@�~�@���@���@�(�@�  @��
@�l�@�A�@��@�/@��@�ƨ@�K�@�v�@�@��7@��@��/@���@�Q�@��@��\@�M�@��T@���@��7@��@�`B@�/@�%@��9@��@�Q�@� �@�b@�1@��w@��@�;d@�@���@���@��\@�^5@�$�@��T@��7@�/@��@��/@��9@�bN@�1@��m@��w@�|�@�S�@��@��@��H@���@��\@�M�@�{@��-@���@��7@�X@�X@�/@�%@�Z@�9X@��@���@��;@���@�\)@�K�@�+@��@��@��!@���@�~�@�ff@�J@���@��h@��7@�p�@�/@��@���@��j@���@�A�@�@\)@+@~�y@~v�@~$�@}�-@}`B@}V@|�j@|j@{�m@{��@{�@{C�@z�\@z^5@z^5@z-@y��@yG�@y&�@y�@x�`@x�u@xA�@w�@w|�@w
=@vȴ@vv�@u��@u�@up�@u`B@uO�@u�@uV@t�D@s�@s"�@r~�@q��@q�7@q7L@p�`@pbN@o��@o�@o;d@nff@n@m�@l�@l(�@k�F@k��@k�@kS�@k@j�\@j�@i�@i�#@i�^@i�7@h��@h��@hbN@h  @g�@g|�@gK�@f��@f5?@f{@e��@e`B@e/@eV@d��@dj@d9X@cƨ@c"�@c@b�H@b�!@a�@a��@ahs@a�@`�`@`��@` �@_l�@_�@^��@^5?@]�T@]@]��@]?}@\��@\��@\�j@\�@\��@\I�@\1@[��@[t�@[o@Z��@Z�!@Z�!@Z��@Z��@Zn�@Y�#@Yhs@YG�@X��@X�u@Xb@W�w@W|�@WK�@V��@VE�@U��@UO�@T�/@T��@TZ@T1@S�m@S�F@St�@SC�@R�H@R�!@R�\@R~�@R~�@R~�@R^5@RM�@Q�@Q��@QX@Q%@P�`@P��@PbN@PQ�@P �@P  @O��@O\)@N�@N��@NV@N{@M��@Mp�@MO�@M?}@L��@L��@LI�@K��@K�
@Kƨ@K��@KdZ@KC�@K@J��@J��@JM�@JM�@J=q@I�^@IX@I&�@H�9@HbN@HQ�@HA�@H �@G�;@Gl�@Fȴ@F��@F��@F��@Fv�@F5?@E�@E��@E?}@EV@D�/@D��@D��@DZ@C�F@CC�@C"�@B�@B��@B�\@BM�@A��@Ahs@A�@@��@@�@@bN@?�;@?\)@?
=@>ȴ@>V@>@=�-@=�h@<�@<�@<�@;�F@;��@;�@;t�@;dZ@;S�@;33@;o@;o@;@:��@:=q@9�@9��@9&�@8��@8�u@8r�@81'@8b@7�@7�;@7�@7+@6�@6ȴ@6ff@6{@5@5�h@5O�@5V@4�j@4z�@4Z@3��@3��@3o@3o@3@2�H@2�!@2^5@1�@1��@1&�@0�9@0A�@/�@/�;@/�@/|�@/;d@/�@/
=@.�y@.��@.V@.{@.@-�-@-�@-�@,�@,j@,Z@,I�@,Z@,I�@,(�@+�m@+�@+33@*�H@*n�@*-@)��@)�#@)��@)G�@)%@(��@(b@'�;@'�P@'�@&ȴ@&�+@&V@&5?@%�T@%�@%O�@%/@%V@$�/@$��@$j@$9X@$�@#��@#�m@#�
@#�F@#�F@#��@#t�@#dZ@#�@#�@#dZ@#"�@"�@"��@"�\@"~�@"^5@"�@"�@!�^@!x�@!hs@!7L@ ��@ �`@ Ĝ@ Ĝ@ Ĝ@ �9@ ��@ r�@ Q�@ 1'@  �@�;@��@��@�@|�@\)@�@��@�R@��@v�@ff@ff@5?@{@{@{@@��@/@V@��@�@��@�@�@�@��@Z@�@�m@��@dZ@dZ@S�@"�@@�@��@�!@n�@=q@=q@=q@J@��@��@�7@x�@&�@��@��@Ĝ@��@�@ �@  @��@|�@K�@�@�@ȴ@��@E�@�T@��@�-@��@�h@O�@�@��@�j@�@��@��@j@I�@(�@��@��@�m@�
@�F@��@��@S�@33@�@�!@^5@J@��@�#@��@X@7L@�@�`@�9@�u@�@r�@1'@  @�w@l�@\)@;d@
=@�R@��@ff111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B^5B^5B^5B^5B^5B^5B^5B^5B^5B^5B^5B]/B]/B]/B]/B]/B^5BdZB�7B��BŢB�B�B��B�B�B�FB�9B�-B�XB�wB�qBǮB��B�fB��B1BC�BW
BR�BO�BQ�BF�B8RB/B>wBW
B\)B_;BdZBcTBiyBk�Bk�Bm�BjBjBiyBffB`BB\)BZB^5B`BBZBL�B5?B�B{BoBJB1B��B�mB��B�^B�B��B�=Bu�B^5BJ�B7LB�B
��B
�NB
�^B
��B
��B
�JB
�B
gmB
ZB
F�B
�B
	7B	�B	��B	��B	�LB	��B	�hB	z�B	l�B	^5B	Q�B	C�B	:^B	 �B	JB	B��B�B�B�B�sB�HB�B��BB�^B�RB�FB�3B�B��B��B��B��B�7B�+B|�Bw�Bt�Bq�Bk�BhsBcTBaHB]/B\)B^5B^5B\)B[#BZBYBVBR�BVBT�BT�BT�BT�BP�BN�BL�BJ�BE�B@�B@�BA�BA�B@�B;dBL�BO�BL�BK�BJ�BJ�BE�BB�B?}B=qB:^B7LB2-B.B,B49B6FB49B49B33B49B49B49B49B49B49B33B33B2-B1'B2-B49B49B49B6FB49B5?BB�BC�BA�B>wB8RB6FB0!B-B+B)�B(�B(�B(�B(�B&�B%�B%�B%�B&�B&�B&�B'�B+B0!B8RB<jB=qBB�BD�BD�BG�BH�BI�BJ�BJ�BJ�BL�BS�B`BBcTBaHBk�Bv�Bw�B}�B�B�B�B� B�=B�uB��B��B��B��B��B��B��B��B��B��B�B�B�-B�^B��B��B�B�#B�HB�sB�mB�sB�`B�;B�)B�B�#B�#B�HB�sB�B��B	B	B	%B	+B	1B	DB	VB	hB	hB	uB	uB	{B	�B	�B	�B	�B	 �B	+B	2-B	49B	7LB	33B	49B	2-B	1'B	49B	:^B	<jB	D�B	E�B	H�B	J�B	I�B	H�B	I�B	K�B	O�B	P�B	R�B	Q�B	YB	[#B	[#B	\)B	^5B	^5B	aHB	cTB	dZB	e`B	hsB	iyB	jB	n�B	q�B	s�B	t�B	y�B	|�B	~�B	~�B	�B	�B	�B	�B	�B	�B	�+B	�1B	�=B	�PB	�PB	�PB	�PB	�PB	�\B	�bB	�bB	�hB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�\B	�VB	�\B	�bB	�hB	�bB	�oB	��B	��B	��B	��B	�{B	�oB	�uB	�oB	�PB	�DB	�JB	�VB	�PB	�bB	��B	��B	��B	��B	��B	�B	�?B	�RB	�^B	�LB	�FB	�3B	�'B	�3B	�9B	�9B	�?B	�?B	�'B	�B	�!B	�'B	�-B	�-B	�-B	�-B	�3B	�9B	�FB	�LB	�RB	�XB	�XB	�^B	�dB	�jB	�}B	��B	��B	ÖB	ĜB	ŢB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�
B	�B	�B	�B	�)B	�)B	�)B	�/B	�/B	�5B	�;B	�NB	�NB	�NB	�TB	�TB	�ZB	�`B	�`B	�fB	�fB	�fB	�mB	�mB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
1B
	7B
	7B

=B

=B
	7B
DB
JB
JB
JB
JB
JB
JB
JB
JB
PB
PB
PB
PB
PB
VB
VB
VB
VB
VB
VB
\B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
!�B
"�B
"�B
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
&�B
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
(�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
+B
+B
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
1'B
1'B
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
6FB
6FB
7LB
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
:^B
:^B
;dB
;dB
;dB
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
=qB
=qB
=qB
=qB
=qB
=qB
>wB
>wB
?}B
?}B
?}B
?}B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
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
F�B
F�B
F�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
I�B
I�B
I�B
J�B
J�B
J�B
K�B
J�B
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
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
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
XB
XB
YB
YB
ZB
ZB
ZB
ZB
ZB
ZB
[#B
\)B
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
]/B
]/B
]/B
]/B
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
`BB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
bNB
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
bNB
bNB
bNB
bNB
cTB
cTB
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
hsB
iyB
iyB
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
l�B
l�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B_B_!B_B_!B_B_!B_!B_!B_!B_B_!B_B_B_B_B_B_!B_!B_!B_B^B^B^B^B^B^B^B^B^B^5B^B\�B]B]IB]�B^�BaHBkB��B�tBƨBچB�-B B�B�/B�8B�LB�?B��BB�BʌB�
B�
B�jBfBD�BX+BS�BQBSuBH1B9rB/�B?BX+B\�B_�Be,Bd&BjKBlqBlqBn}BkBk6Bj�Bh$Ba�B]~B[WB_�BbNB\�BO�B7�B�BMB[B�B
�B��B�B�B��B�cB�sB��By	B`vBMjB:�B�B
�"B
�B
��B
�LB
�:B
�pB
�gB
i�B
^B
KDB
!HB
"B	�[B	οB	�aB	�xB	��B	��B	}VB	n�B	`�B	TB	F?B	>�B	$B	pB	�B��B�MB�;B�B�KB��B�+BѷBÖB��B��B�fB�nB��B��B�>B�:B��B�xB�	B~]By�BwBs�BmwBi�BdZBb�B^�B]dB_VB_!B\�B[�BZ�BZBW?BTFBVSBU2BU�BV�BV�BQ�BO�BM�BL0BGBBBA�BB[BB�BAUB;�BNBP�BMPBL�BL�BL�BF�BC{B@OB>]B;�B8�B4B.�B,�B5?B6�B4�B4�B3�B4�B4�B4�B4�B5B4�B3�B3�B2�B2aB3�B4�B4�B5ZB7�B4�B5�BCGBD�BC�B@iB9�B7�B0UB-CB+�B+QB)�B)_B)DB)_B'�B&LB&B&fB'�B'�B'�B(�B+B0B8RB<�B=�BB�BEBEBG�BIBI�BJ�BJ�BJ�BLdBS�B`vBcnB`�BkBv�Bw�B~]B��B��B��B�B��B�uB��B��B�)B��B��B�~B�qB�\B�tB��B�B��B��B��B��BΊBՁB��B�-B��B�XB��B�B�BBܬB�kB�	B��B��B�$B��B�ZB	B	MB	%B	EB	fB	^B	VB	�B	�B	�B	�B	�B	�B	�B	�B	�B	 vB	*�B	2aB	4�B	7�B	3�B	4�B	2aB	0�B	3�B	:DB	<6B	DgB	E�B	H�B	K)B	J=B	H�B	I�B	LB	O�B	QhB	SuB	Q�B	YB	[	B	[=B	\]B	^5B	^5B	a-B	cTB	dtB	e�B	h�B	i_B	jeB	ncB	qvB	s�B	t�B	y�B	|�B	~�B	B	�B	��B	�B	�B	�9B	�9B	�EB	�KB	�=B	�jB	�PB	�6B	�6B	�PB	�BB	�.B	�HB	�NB	�[B	�{B	��B	�mB	�mB	�mB	�mB	��B	��B	�sB	��B	��B	�KB	��B	��B	�"B	�BB	�bB	��B	�.B	�TB	��B	��B	��B	��B	��B	�oB	�B	��B	��B	�DB	�dB	��B	��B	��B	��B	��B	��B	��B	�_B	��B	�B	�lB	��B	��B	��B	�hB	�'B	�MB	�9B	�9B	��B	��B	�[B	�5B	�;B	�'B	�B	�B	�B	�-B	�3B	�9B	�FB	�LB	�8B	�$B	�$B	�^B	�dB	�jB	�}B	�oB	��B	ÖB	ĜB	ňB	ƨB	��B	ɺB	ʦB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	��B	�B	�B	�B	�B	�B	�/B	�5B	�pB	�4B	�4B	�4B	�:B	�TB	�ZB	�,B	�FB	�LB	�fB	�fB	�RB	�RB	�XB	�B	�B	�kB	�kB	�kB	�B	�B	�wB	�wB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B	��B
 �B
B
�B
�B
�B
�B
�B
�B
B
-B
B
B
B
%B
B
+B
B
1B
	B
	7B

XB

=B
	7B
^B
JB
0B
0B
0B
0B
JB
0B
JB
6B
6B
6B
6B
PB
<B
<B
VB
"B
<B
<B
\B
BB
BB
BB
HB
.B
.B
HB
HB
4B
NB
oB
@B
[B
[B
{B
MB
gB
gB
mB
mB
�B
�B
yB
�B
B
B
B
B
�B
�B
�B
kB
kB
�B
�B
�B
�B
�B
�B
�B
xB
]B
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
!�B
"�B
"�B
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
&�B
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
(�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
*�B
+B
+�B
+�B
+�B
+�B
+�B
,B
,�B
,�B
-�B
-�B
-�B
-�B
-�B
-�B
-�B
/ B
.�B
.�B
/B
/�B
0B
0!B
0�B
0�B
0�B
1B
1B
2-B
2-B
2�B
2�B
3B
3B
4B
4B
4B
5%B
5%B
5%B
5%B
5%B
6B
6FB
72B
7B
88B
88B
8B
8B
9XB
:*B
:DB
:DB
:DB
:DB
:^B
;JB
;JB
;JB
;dB
;JB
;JB
;JB
<jB
<PB
<jB
<6B
=<B
=<B
=<B
=<B
=<B
=<B
=VB
=<B
=VB
=VB
>wB
>BB
?cB
?cB
?HB
?HB
@OB
@iB
@OB
AUB
AoB
AoB
A�B
BuB
BuB
B�B
DgB
D�B
DgB
DgB
D�B
E�B
E�B
E�B
E�B
E�B
E�B
FtB
F�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
I�B
I�B
I�B
J�B
J�B
J�B
K�B
J�B
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
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
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
W�B
W�B
X�B
X�B
Y�B
ZB
ZB
ZB
ZB
ZB
[	B
[�B
[	B
[	B
[	B
\B
\B
\�B
\�B
\�B
\�B
\�B
]B
]B
]B
\�B
^B
^B
^B
^B
^B
^B
^B
_!B
_!B
_!B
_!B
_!B
^�B
_B
_!B
`B
`B
`B
`'B
`BB
`BB
a-B
aB
`B
aB
aB
`�B
`�B
a-B
a-B
a-B
b4B
aB
b4B
bB
b4B
bB
bB
bB
bB
b4B
b4B
b4B
bB
bB
b4B
c:B
c B
c:B
c:B
c:B
c:B
d@B
dB
d@B
d&B
d@B
d@B
d&B
d&B
eFB
eFB
eFB
eFB
e,B
eFB
eFB
e,B
f2B
f2B
fLB
fLB
fLB
gRB
gRB
g8B
g8B
gB
g8B
g8B
gRB
gRB
g8B
g8B
g8B
hXB
h>B
h>B
hXB
hXB
hXB
hXB
i_B
i_B
i_B
iDB
jeB
jKB
jeB
jeB
kkB
kkB
kkB
kQB
kkB
kkB
kQB
kkB
lqB
lWB
lWB
lqB
lqB
lWB
lqB
lW111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.43(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201912080035172019120800351720191208003517202306231719202023062317192020230623171920201912090031492019120900314920191209003149  JA  ARFMdecpA19c                                                                20191203033722  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20191202183728  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20191202183730  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20191202183731  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20191202183732  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20191202183732  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20191202183732  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20191202183732  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20191202183734  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20191202183734                      G�O�G�O�G�O�                JA  ARUP                                                                        20191202185444                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20191204154357  CV  JULD            G�O�G�O�FǃM                JM  ARCAJMQC2.0                                                                 20191207153517  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20191207153517  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20191208153149  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20230623081920  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20230705031507                      G�O�G�O�G�O�                