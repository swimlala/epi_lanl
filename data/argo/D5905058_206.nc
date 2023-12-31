CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2020-01-27T06:38:21Z creation;2020-01-27T06:38:27Z conversion to V3.1;2023-06-29T05:50:09Z update;     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20200127063821  20230705031507  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0675_206                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @��I� 1   @��I�-� @7U�8�YK�b�e��O1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�  @�  @���A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BHffBP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$�C&  C(�C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2fD2� D3  D3�fD4  D4� D5fD5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DFfDF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]fD]�fD^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�<�Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�  D�@ D� D�� D�  D�@ D� D��D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��
@��
AQ�A)�AI�Ai�A���A���A���A���A���A���A���A���Bz�B
z�Bz�Bz�B"z�B*z�B2z�B:z�BBz�BJ�HBRz�BZz�Bbz�Bjz�Brz�Bzz�B�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�p�B�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qC ��C��C��C��C��C
��C��C��C��C��C��C��C��C��C��C��C ��C"��C$�RC&��C(�RC*��C,��C.��C0��C2��C4��C6��C8��C:��C<��C>��C@��CB��CD��CF��CH��CJ��CL��CN��CP��CR��CT��CV��CX��CZ��C\��C^��C`��Cb��Cd��Cf��Ch��Cj��Cl��Cn��Cp��Cr��Ct��Cv��Cx��Cz��C|��C~��C�O\C�B�C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�\)C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\D '�D ��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D	'�D	��D
'�D
��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D '�D ��D!'�D!��D"'�D"��D#'�D#��D$'�D$��D%'�D%��D&'�D&��D''�D'��D('�D(��D)'�D)��D*'�D*��D+'�D+��D,'�D,��D-'�D-��D.'�D.��D/'�D/��D0'�D0��D1'�D1��D2.D2��D3'�D3�D4'�D4��D5.D5��D6'�D6��D7'�D7��D8'�D8��D9'�D9��D:'�D:��D;'�D;��D<'�D<��D='�D=��D>'�D>��D?'�D?��D@'�D@��DA'�DA��DB'�DB��DC'�DC��DD'�DD��DE'�DE��DF.DF��DG'�DG��DH'�DH��DI'�DI��DJ'�DJ��DK'�DK��DL'�DL��DM'�DM��DN'�DN��DO'�DO��DP'�DP��DQ'�DQ��DR'�DR��DS'�DS��DT'�DT��DU'�DU��DV'�DV��DW'�DW��DX'�DX��DY'�DY��DZ'�DZ��D['�D[��D\'�D\��D].D]�D^'�D^��D_'�D_��D`'�D`��Da'�Da��Db'�Db��Dc'�Dc��Dd'�Dd��De'�De��Df'�Df��Dg'�Dg��Dh'�Dh��Di'�Di��Dj'�Dj��Dk'�Dk��Dl'�Dl��Dm'�Dm��Dn'�Dn��Do'�Do��Dp'�Dp��Dq'�Dq��Dr'�Dr��Ds'�Ds��Dt'�Dt��Du'�Du��Dv'�Dv��Dw'�Dw��Dx'�Dx��Dy'�Dy��Dz'�Dz��D{'�D{��D|'�D|��D}'�D}��D~'�D~��D'�D��D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�W
D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D��
D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D�D���D��D�S�DÓ�D���D��D�S�Dē�D���D��D�S�Dœ�D���D��D�S�DƓ�D���D��D�S�DǓ�D���D��D�S�Dȓ�D���D��D�P�Dɓ�D���D��D�S�Dʓ�D���D��D�S�D˓�D���D��D�S�D̓�D���D��D�S�D͓�D���D��D�S�DΓ�D���D��D�S�Dϓ�D���D��D�S�DГ�D���D��D�S�Dѓ�D���D��D�S�Dғ�D���D��D�S�Dӓ�D���D��D�S�Dԓ�D���D��D�S�DՓ�D���D��D�S�D֓�D���D��D�S�Dד�D���D��D�S�Dؓ�D���D��D�S�Dٓ�D���D��D�S�Dړ�D���D��D�S�Dۓ�D���D��D�S�Dܓ�D���D��D�S�Dݓ�D���D��D�S�Dޓ�D���D��D�S�Dߓ�D���D��D�S�D���D���D��D�S�D��D���D��D�S�D��D���D��D�S�D��D���D��D�S�D��D���D��D�S�D��D���D��D�S�D��D���D��D�S�D��D��
D��D�S�D��D���D��D�S�D��D�ФD��D�S�D��D���D��D�S�D��D���D��D�S�D��D���D��D�S�D��D���D��D�S�D��D���D��D�S�D��D���D��D�S�D��D���D��D�S�D��D��
D��D�S�D��D���D��D�S�D��D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D��=111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�7LA�;dA�7LA�(�A�VA�
=A�
=A�%A��A��yA��`A��TA��`A��TA��TA��TA��HA��;A��HA��HA��TA��TA��TA��TA��TA��TA��HA��HA��HA��;A��;A��/A��;A��HA��#A��A���A���A��wA��FA��!A���A��uA��A�l�A�Q�A�G�A�?}A�7LA��A��A�p�A��FA��A�jA���A���A��yA�bA��!A�ffA��TA���A���A�E�A�/A�O�A���A��A�`BA��#A�ƨA��hA���A���A���A��A�^5A�VA���A�7LA�$�A��\A�G�A�r�A��7A���A�7LA���A��A��#A�  A�{A��^A���A�{A�|�A�7LA���A��PA��^A���A�1A�G�A��#A�E�A���A��A|�A}��Az��AwƨAv^5At��Ar�An��Am7LAlVAj�RAh��Ag�#Ag�Ae�^AcVA_S�A^$�A];dA\�`A\~�AZ�/AXz�AV��ATjAR��AN�HAL1'AJ�jAIl�AH��AF�/AES�AC�wAB1A?��A>��A=��A=S�A;�mA:-A9K�A8ZA6�!A5"�A4ffA2�9A1&�A01'A/�hA/"�A.ȴA.�+A.JA-x�A,�A,(�A+x�A*��A)�A(��A'33A&��A&A�A%
=A$(�A#XA"z�A!�A!A �AC�A��A�;A"�A~�A^5A�A1A�A��AoAhsA �A��A33AK�A�Al�A;dA��A��A�A�+A�A=qA�^AG�AoA
��A
5?A	�TA	C�A�A��A��A��A�A-A&�A r�@�t�@��\@���@���@�Ĝ@���@�=q@�7L@��@��@�p�@��#@��;@�S�@��y@�h@�Q�@���@�^5@��@���@��@�I�@�  @�+@݁@�j@�t�@��H@�n�@�M�@�-@�x�@ش9@�ƨ@��@��@�M�@�{@���@���@�%@��@ӍP@�K�@��@ёh@Гu@�t�@Η�@��@�X@�7L@��m@�p�@ȃ@��@ǶF@�t�@�"�@���@���@Ɨ�@���@��`@�r�@�9X@��@�A�@Ý�@��y@°!@�ff@�7L@��w@��@�~�@�^5@��@�Q�@�?}@��@��/@�o@��9@���@�~�@��/@�1@�;d@�t�@�S�@��h@�V@�ff@�K�@���@���@��#@��/@�o@���@� �@�j@�Ĝ@�|�@��\@�%@��@��+@��@�7L@��@�ƨ@��u@�Q�@�5?@�~�@�x�@�o@�l�@�
=@�33@��;@�|�@�$�@��T@�K�@���@�I�@�@�+@�t�@�{@�1'@��;@��@��@��@�?}@���@���@��j@�z�@���@���@��#@�G�@��@���@��;@�=q@�@���@��7@�G�@�9X@��m@�|�@�|�@��H@��w@�n�@�-@��@��@���@��@��@��
@��m@���@��9@���@�?}@�X@��F@��@�?}@�Q�@�Q�@��@���@��F@�\)@�"�@�C�@�o@�+@�+@���@�
=@�|�@�J@��@�G�@��/@��@�1'@���@��@���@��y@��@��@��!@�E�@��#@�O�@���@��@���@� �@��
@��P@�l�@�\)@�@��+@�$�@��@���@�@��^@��7@�?}@�&�@�/@�Ĝ@���@��9@�Z@�p�@��/@�(�@���@���@���@��H@�-@�{@��^@��h@���@���@��7@�p�@�X@�%@���@�Z@�b@��m@�dZ@���@���@��+@�V@�E�@��@���@�`B@�/@��@���@��@�r�@�A�@��m@��@�|�@�;d@�@��y@��@��!@��+@�E�@�{@�J@��@���@�@��^@�p�@�&�@�&�@�&�@��@��@�Ĝ@���@��@�1'@�b@�;@�w@K�@~�R@~@}�h@}�@|�/@|�j@|��@|I�@{�
@{t�@{"�@z��@z�\@z�@y��@yx�@y�@x��@x�u@xbN@w��@w�P@w|�@w+@v��@v5?@u�T@u`B@t��@t�@tj@t(�@s�m@s��@sS�@s"�@r�H@r=q@q�^@qX@q�@p�9@pA�@o��@o|�@o\)@n�@n�+@nE�@n{@m��@m�h@mO�@m?}@l��@l��@l�D@lj@l(�@k��@kC�@k33@k33@j�H@j��@j-@i�^@i��@ihs@i7L@h�u@hbN@h  @g�@g;d@fȴ@fff@f5?@e�T@e/@d�@d�D@d�D@dZ@c�m@c�F@c33@b��@b�!@b^5@b-@a��@a�@a%@`Ĝ@`b@_��@_�P@_
=@^ff@^5?@]��@]�@]?}@\I�@[�@[33@Z�!@Z^5@ZJ@Yhs@Y&�@X��@X�u@X1'@W�@W��@Wl�@V�R@V��@VE�@U�@U��@U�@T�@T�/@T��@T�@S�
@St�@So@R��@R-@Q�@Qhs@Q&�@P��@PĜ@PĜ@P��@P�u@Pr�@PA�@O�@O�w@O�@O|�@O;d@O+@N�@N��@NV@M�-@MO�@L�@L�D@LI�@L1@K�m@Kƨ@K��@K�@K33@J�@J��@J=q@J-@I�@Ix�@IG�@H�@HA�@G�@G�@G\)@F��@F�R@F��@Fff@F5?@E��@E?}@EV@D��@D�j@Dz�@C��@C�@B�@B�!@Bn�@BJ@A��@A�@A�@A�#@A��@A��@A��@A��@A��@A�7@Ahs@A7L@@��@@��@@r�@@1'@?�;@?��@?��@?��@?��@?�P@?l�@?;d@?;d@?
=@>V@>@=�T@=@=O�@<�@<�D@<(�@;�
@;t�@;@:��@:~�@9�@9&�@8��@8Ĝ@8�9@8r�@8 �@7��@7�P@7\)@7+@6��@6�+@65?@6$�@6@5@5�h@5O�@4��@4j@4(�@41@3�F@3��@3��@3�@3t�@3o@2��@2�!@2n�@2J@1�^@1G�@0�`@0�u@0�@0bN@/�@/�w@/�@/�@/�@/+@.ȴ@.�+@.v�@.5?@-�@-�T@-�@-V@,��@,�@,��@,Z@,�@+�
@+��@+�@+o@*�!@*�@)��@)x�@)7L@(�`@(�@(bN@(1'@(b@'�;@'�P@'l�@';d@'
=@&ȴ@&�R@&v�@&E�@%@%�-@%`B@%/@$��@$�@$�D@$z�@$Z@$�@#�
@#��@#o@"��@"��@"~�@"n�@"=q@"J@!��@!G�@!%@ �9@ ��@ r�@ A�@   @�@��@�P@�@�R@�+@v�@�T@�-@�h@�@`B@/@V@�/@�D@I�@(�@1@��@��@�@t�@t�@t�@dZ@C�@o@�@�H@�\@^5@J@��@x�@X@G�@7L@&�@�`@��@r�@�@�@�P@\)@+@�@ȴ@��@E�@{@{@{@�T@��@�@?}@�@��@��@Z@9X@��@�m@ƨ@��@dZ@33@o@�@��@��@�!@n�@-@��@�#@�^@��@��@x�@G�@�@%@��@�`@Ĝ@r�@bN@A�@  @�w@|�@�@�R@�R@��@�+@V@{@�T@��@p�@O�@?}@V@��@�/@�j@��@j@9X@1@�
@��@�@S�@33@"�@"�@o@
�@
��@
��@
��@
~�@
n�@
^5@
^5@
�@	��@	�^@	��@	�7@	X@	&�@	%@�`@Ĝ@��@�@bN@Q�@1'@b111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�7LA�;dA�7LA�(�A�VA�
=A�
=A�%A��A��yA��`A��TA��`A��TA��TA��TA��HA��;A��HA��HA��TA��TA��TA��TA��TA��TA��HA��HA��HA��;A��;A��/A��;A��HA��#A��A���A���A��wA��FA��!A���A��uA��A�l�A�Q�A�G�A�?}A�7LA��A��A�p�A��FA��A�jA���A���A��yA�bA��!A�ffA��TA���A���A�E�A�/A�O�A���A��A�`BA��#A�ƨA��hA���A���A���A��A�^5A�VA���A�7LA�$�A��\A�G�A�r�A��7A���A�7LA���A��A��#A�  A�{A��^A���A�{A�|�A�7LA���A��PA��^A���A�1A�G�A��#A�E�A���A��A|�A}��Az��AwƨAv^5At��Ar�An��Am7LAlVAj�RAh��Ag�#Ag�Ae�^AcVA_S�A^$�A];dA\�`A\~�AZ�/AXz�AV��ATjAR��AN�HAL1'AJ�jAIl�AH��AF�/AES�AC�wAB1A?��A>��A=��A=S�A;�mA:-A9K�A8ZA6�!A5"�A4ffA2�9A1&�A01'A/�hA/"�A.ȴA.�+A.JA-x�A,�A,(�A+x�A*��A)�A(��A'33A&��A&A�A%
=A$(�A#XA"z�A!�A!A �AC�A��A�;A"�A~�A^5A�A1A�A��AoAhsA �A��A33AK�A�Al�A;dA��A��A�A�+A�A=qA�^AG�AoA
��A
5?A	�TA	C�A�A��A��A��A�A-A&�A r�@�t�@��\@���@���@�Ĝ@���@�=q@�7L@��@��@�p�@��#@��;@�S�@��y@�h@�Q�@���@�^5@��@���@��@�I�@�  @�+@݁@�j@�t�@��H@�n�@�M�@�-@�x�@ش9@�ƨ@��@��@�M�@�{@���@���@�%@��@ӍP@�K�@��@ёh@Гu@�t�@Η�@��@�X@�7L@��m@�p�@ȃ@��@ǶF@�t�@�"�@���@���@Ɨ�@���@��`@�r�@�9X@��@�A�@Ý�@��y@°!@�ff@�7L@��w@��@�~�@�^5@��@�Q�@�?}@��@��/@�o@��9@���@�~�@��/@�1@�;d@�t�@�S�@��h@�V@�ff@�K�@���@���@��#@��/@�o@���@� �@�j@�Ĝ@�|�@��\@�%@��@��+@��@�7L@��@�ƨ@��u@�Q�@�5?@�~�@�x�@�o@�l�@�
=@�33@��;@�|�@�$�@��T@�K�@���@�I�@�@�+@�t�@�{@�1'@��;@��@��@��@�?}@���@���@��j@�z�@���@���@��#@�G�@��@���@��;@�=q@�@���@��7@�G�@�9X@��m@�|�@�|�@��H@��w@�n�@�-@��@��@���@��@��@��
@��m@���@��9@���@�?}@�X@��F@��@�?}@�Q�@�Q�@��@���@��F@�\)@�"�@�C�@�o@�+@�+@���@�
=@�|�@�J@��@�G�@��/@��@�1'@���@��@���@��y@��@��@��!@�E�@��#@�O�@���@��@���@� �@��
@��P@�l�@�\)@�@��+@�$�@��@���@�@��^@��7@�?}@�&�@�/@�Ĝ@���@��9@�Z@�p�@��/@�(�@���@���@���@��H@�-@�{@��^@��h@���@���@��7@�p�@�X@�%@���@�Z@�b@��m@�dZ@���@���@��+@�V@�E�@��@���@�`B@�/@��@���@��@�r�@�A�@��m@��@�|�@�;d@�@��y@��@��!@��+@�E�@�{@�J@��@���@�@��^@�p�@�&�@�&�@�&�@��@��@�Ĝ@���@��@�1'@�b@�;@�w@K�@~�R@~@}�h@}�@|�/@|�j@|��@|I�@{�
@{t�@{"�@z��@z�\@z�@y��@yx�@y�@x��@x�u@xbN@w��@w�P@w|�@w+@v��@v5?@u�T@u`B@t��@t�@tj@t(�@s�m@s��@sS�@s"�@r�H@r=q@q�^@qX@q�@p�9@pA�@o��@o|�@o\)@n�@n�+@nE�@n{@m��@m�h@mO�@m?}@l��@l��@l�D@lj@l(�@k��@kC�@k33@k33@j�H@j��@j-@i�^@i��@ihs@i7L@h�u@hbN@h  @g�@g;d@fȴ@fff@f5?@e�T@e/@d�@d�D@d�D@dZ@c�m@c�F@c33@b��@b�!@b^5@b-@a��@a�@a%@`Ĝ@`b@_��@_�P@_
=@^ff@^5?@]��@]�@]?}@\I�@[�@[33@Z�!@Z^5@ZJ@Yhs@Y&�@X��@X�u@X1'@W�@W��@Wl�@V�R@V��@VE�@U�@U��@U�@T�@T�/@T��@T�@S�
@St�@So@R��@R-@Q�@Qhs@Q&�@P��@PĜ@PĜ@P��@P�u@Pr�@PA�@O�@O�w@O�@O|�@O;d@O+@N�@N��@NV@M�-@MO�@L�@L�D@LI�@L1@K�m@Kƨ@K��@K�@K33@J�@J��@J=q@J-@I�@Ix�@IG�@H�@HA�@G�@G�@G\)@F��@F�R@F��@Fff@F5?@E��@E?}@EV@D��@D�j@Dz�@C��@C�@B�@B�!@Bn�@BJ@A��@A�@A�@A�#@A��@A��@A��@A��@A��@A�7@Ahs@A7L@@��@@��@@r�@@1'@?�;@?��@?��@?��@?��@?�P@?l�@?;d@?;d@?
=@>V@>@=�T@=@=O�@<�@<�D@<(�@;�
@;t�@;@:��@:~�@9�@9&�@8��@8Ĝ@8�9@8r�@8 �@7��@7�P@7\)@7+@6��@6�+@65?@6$�@6@5@5�h@5O�@4��@4j@4(�@41@3�F@3��@3��@3�@3t�@3o@2��@2�!@2n�@2J@1�^@1G�@0�`@0�u@0�@0bN@/�@/�w@/�@/�@/�@/+@.ȴ@.�+@.v�@.5?@-�@-�T@-�@-V@,��@,�@,��@,Z@,�@+�
@+��@+�@+o@*�!@*�@)��@)x�@)7L@(�`@(�@(bN@(1'@(b@'�;@'�P@'l�@';d@'
=@&ȴ@&�R@&v�@&E�@%@%�-@%`B@%/@$��@$�@$�D@$z�@$Z@$�@#�
@#��@#o@"��@"��@"~�@"n�@"=q@"J@!��@!G�@!%@ �9@ ��@ r�@ A�@   @�@��@�P@�@�R@�+@v�@�T@�-@�h@�@`B@/@V@�/@�D@I�@(�@1@��@��@�@t�@t�@t�@dZ@C�@o@�@�H@�\@^5@J@��@x�@X@G�@7L@&�@�`@��@r�@�@�@�P@\)@+@�@ȴ@��@E�@{@{@{@�T@��@�@?}@�@��@��@Z@9X@��@�m@ƨ@��@dZ@33@o@�@��@��@�!@n�@-@��@�#@�^@��@��@x�@G�@�@%@��@�`@Ĝ@r�@bN@A�@  @�w@|�@�@�R@�R@��@�+@V@{@�T@��@p�@O�@?}@V@��@�/@�j@��@j@9X@1@�
@��@�@S�@33@"�@"�@o@
�@
��@
��@
��@
~�@
n�@
^5@
^5@
�@	��@	�^@	��@	�7@	X@	&�@	%@�`@Ĝ@��@�@bN@Q�@1'@b111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  BuBuBuBoBbBbBbBbBVBVBVBVBVB\B\B\BbBbBbBbBbBbBhBhBhBoBoBoBoBhBhBhBoBuBuBuB{B{B�B�B�B�B�B�B�B�B�B�B�B�B�B!�B?}B��B�?BƨBȴB��B��B�B�mB��B�B�B$�B,B%�B$�B'�B5?B@�BI�BE�B@�B?}B/B&�B�B��B��B��B��B��B�B��B�B��B��B�B�ZB�)B��B�}B��B�BaHBVB;dB"�B
��B
�sB
�
B
��B
p�B
\)B
D�B
;dB
1'B
,B
$�B
�B
	7B	��B	�B	�mB	��B	�}B	�FB	��B	��B	�\B	�DB	�1B	}�B	^5B	R�B	H�B	C�B	>wB	2-B	"�B	�B		7B��B�HBȴB�wB�FBB�jB�'B�B��B��B�hB�PB�DB�DB�B� Bz�By�Bw�Bw�Bz�Bw�Bu�Bs�Br�Bq�Bp�Bo�Bn�Bm�Bk�Bk�BiyBgmBgmBbNBaHBbNB`BB\)B[#BYBXBXBXBW
BVBW
B^5B]/B`BBbNB^5B[#BYBXBe`Be`B]/BZBR�BP�BVBXBZBW
BXB[#Bt�Bx�Bw�Bv�Bw�By�Bx�Bv�Bu�Bt�Br�Bn�BjBbNB\)B]/B\)BZB]/BXBS�BR�BN�BL�BO�BP�BN�BN�BP�BP�BP�BQ�BQ�BQ�BP�BO�BO�BP�BP�BO�BP�BQ�BT�BXB^5B_;B`BB`BB`BBaHBbNBaHBaHBbNBdZBe`BiyBr�Bw�By�Bx�Bx�Bx�Bw�Bu�Bv�Bu�Bt�Br�Br�Bu�Bs�Bs�Br�Br�Br�Bs�Bs�Bs�Bt�Bw�Bz�B|�B}�B~�B�B�B�B�B�B�B�B�B� B~�B�B�=B��B��B��B��B�{B�hB�bB��B��B��B��B��B��B��B��B�-B�^B�jB�^B�}B�jB��BƨB��B�B�B��B��B��B��B�B�BB�B��B�#B�/B�B��B��B�B��B��B��B	+B	JB	
=B	1B	{B	�B	\B	$�B	(�B	,B	+B	$�B	%�B	/B	2-B	9XB	A�B	A�B	B�B	D�B	D�B	C�B	B�B	B�B	C�B	B�B	C�B	F�B	A�B	A�B	A�B	B�B	B�B	B�B	E�B	D�B	H�B	K�B	R�B	VB	W
B	cTB	gmB	r�B	r�B	o�B	o�B	o�B	p�B	w�B	{�B	~�B	�B	~�B	z�B	w�B	s�B	u�B	y�B	}�B	|�B	{�B	}�B	�B	�B	�1B	�DB	�bB	�VB	�uB	�uB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�-B	�3B	�9B	�?B	�FB	�LB	�XB	�dB	�qB	�}B	B	ÖB	B	��B	��B	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�
B	�B	�B	�B	�B	�#B	�)B	�)B	�/B	�5B	�;B	�BB	�BB	�HB	�NB	�TB	�ZB	�`B	�fB	�fB	�fB	�mB	�mB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
1B
1B
	7B
	7B
	7B

=B

=B
DB
DB
DB
JB
JB
JB
JB
JB
PB
PB
PB
VB
PB
VB
VB
\B
VB
\B
\B
bB
bB
hB
hB
hB
oB
oB
uB
uB
uB
uB
uB
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
!�B
!�B
"�B
"�B
"�B
$�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
&�B
'�B
'�B
(�B
(�B
)�B
)�B
)�B
+B
+B
+B
,B
,B
,B
,B
,B
-B
-B
-B
-B
.B
.B
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
/B
0!B
0!B
0!B
0!B
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
49B
49B
5?B
5?B
5?B
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
8RB
8RB
8RB
8RB
8RB
8RB
9XB
9XB
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
=qB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
@�B
@�B
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
B�B
B�B
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
N�B
N�B
N�B
N�B
O�B
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
XB
XB
XB
XB
YB
YB
YB
YB
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
[#B
\)B
\)B
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
l�B
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
m�B
n�B
n�B
n�B
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
q�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B@B[B[BoBHB.BHBbB"B"B"B"B"B(B(B(B.B.B.B.B.B.B4B4B4B:B:B:B:B4B4B4B:B@B@B[BaBaBgBgBgB�B�B�B�BsByB�B�BQB�B&�BE�B��B��BȀBʦB��BѝB�xB��B��B�B�B(�B/5B'B&�B)DB6�BB�BL0BG�BC-BB�B1�B)yBYB�wB��B�2B�FB�LB�|B��B�B�<B�FB�B��B�B� B��B�/B�%BcTBX�B>�B'B
�]B
�B
�IB
��B
tTB
_�B
G�B
<�B
2B
.cB
($B
 vB

�B	�B	�B	�B	�\B	��B	�B	��B	��B	��B	�PB	�xB	�;B	_pB	S�B	IB	DgB	@�B	4�B	%,B	7B	�B��B��BʌB��B��BĜB�BB�B�/B�$B��B�oB�"B�B�B�B�;B|�B{By	By�B|jBx�BvFBtBr�Bq�Bq'Bp!Bo5BncBlqBlqBjeBh�Bh�Bb�Bb4Bc�Ba-B]B[�BY�BYBX�BX�BW�BW
BW�B^�B]~BabBc�B_�B\]BYBXEBf�BgB_!B\)BT,BQ�BV9BX�B[#BW�BX+BZ�Bu?By>BxBv�BxBz^By>Bw�Bv�Bu�Bs�Bo�Bl�Bd&B]IB]�B\�B[	B_BX�BT�BUBPBMjBP}BQ�BO�BP�BQ�BQBQ4BR�BR�BR�BQBP.BP.BP�BP�BO�BQhBR�BU�BXyB^OB_;B`'B`BB`�Ba�Bb�Ba|BaHBb�BdZBe`Bi�Br�Bx8By�Bx�By	ByrBxRBvFBwBu�Bt�Br�Bs�Bv�BtBs�Br�Br�Br�Bs�Bs�Bs�BuBxBz�B|�B}�B~�B�GB�9B�B�;B��B��B�UB� B� BcB�-B��B�B��B��B��B��B��B��B��B��B�MB��B�CB�_B��B�8B��B��B��B��B� B��B�B�YBѷBٴBևB��BуB��B��BյB�B��B�aBںB�)B�CB�ZB��B�oB��B�B�wB	EB	�B		�B	_B	�B	�B	B	$tB	(�B	,�B	+�B	$�B	%,B	.cB	1�B	9XB	A�B	AoB	B[B	D�B	EB	C�B	B�B	B�B	C{B	B�B	DB	GEB	A�B	AoB	AoB	B�B	B�B	BuB	E�B	DgB	H�B	K^B	S@B	U�B	U�B	b�B	f�B	r�B	r�B	oiB	oOB	oB	o�B	wfB	{�B	~�B	��B	�B	{0B	xB	s�B	utB	y�B	~(B	|�B	{�B	}�B	��B	��B	��B	��B	�}B	�<B	��B	��B	�[B	��B	��B	��B	��B	��B	�kB	�=B	�;B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�B	��B	�B	�B	�B	�B	�$B	�B	�VB	�.B	�[B	�aB	��B	��B	��B	ɺB	�lB	̳B	��B	��B	ΥB	οB	͟B	̈́B	ЗB	��B	��B	��B	�B	�B	��B	��B	��B	�B	�B	��B	��B	��B	��B	�B	��B	�B	�B	�B	�B	�!B	�B	�'B	�HB	�B	�:B	�&B	�FB	�2B	�2B	�LB	�8B	�RB	�>B	�$B	�DB	�DB	�0B	�KB	�eB	�QB	�=B	�=B	�CB	�]B	�wB	�cB	�iB	�B	�oB	�vB	�[B	�B	�|B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
B
B
B
�B
B
B
B
	B
	B
	B

	B

	B
B
B

�B
B
B
B
B
B
B
B
6B
"B
B
B
"B
B
<B
(B
(B
.B
.B
NB
4B
NB
:B
TB
@B
@B
@B
[B
[B
FB
FB
FB
FB
gB
MB
mB
mB
YB
?B
YB
yB
yB
eB
B
B
eB
kB
�B
�B
qB
�B
dB
�B
�B
�B
�B
�B
jB
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
!|B
!�B
"�B
"�B
"�B
$�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
&�B
'�B
'�B
(�B
(�B
)�B
)�B
)�B
*�B
*�B
*�B
+�B
+�B
+�B
+�B
+�B
,�B
,�B
,�B
,�B
-�B
-�B
-�B
-�B
-�B
-�B
-�B
-�B
-�B
.�B
.�B
.�B
.�B
.�B
/�B
0B
/�B
0!B
0�B
0�B
0�B
0�B
1�B
1�B
1�B
1�B
2�B
3B
3B
3�B
3�B
5B
5B
5%B
5%B
6+B
6B
7B
6�B
7B
8B
7�B
8B
7�B
8B
8B
8B
8B
8B
8B
8B
8B
9$B
9$B
9$B
9$B
:*B
:B
;B
:�B
;B
;B
;0B
<B
<6B
<PB
<6B
=<B
=<B
=VB
=VB
=<B
>BB
>BB
>]B
>BB
?HB
?cB
?cB
?cB
@OB
@B
@OB
@4B
@OB
A;B
AUB
AUB
AUB
BAB
B[B
BAB
B[B
BAB
CGB
CaB
CaB
C{B
DMB
DgB
DMB
ESB
ESB
E9B
EmB
ESB
EmB
EmB
FtB
FtB
F�B
F�B
G�B
GzB
G_B
HfB
H�B
H�B
HfB
HfB
IlB
IlB
I�B
I�B
I�B
I�B
IlB
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
N�B
N�B
N�B
N�B
O�B
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
W�B
W�B
W�B
W�B
X�B
X�B
X�B
X�B
Y�B
Y�B
Y�B
Y�B
Y�B
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
[�B
[�B
[�B
[�B
[�B
[�B
[�B
]B
\�B
\�B
\�B
\�B
^B
^B
^B
]�B
^B
_B
_B
_B
_B
_!B
`B
`B
`B
aB
aB
aB
aB
aB
a�B
a�B
bB
bB
bB
bB
b4B
c B
c B
c B
c B
cB
d&B
dB
d&B
d&B
dB
dB
e,B
e,B
e,B
eB
e,B
eB
e,B
f2B
f2B
f2B
fB
fB
f2B
f2B
g8B
gB
gB
g8B
g8B
g8B
g8B
gB
g8B
h>B
h>B
hXB
iDB
i*B
i*B
iDB
iDB
iDB
jKB
jKB
j0B
jKB
k6B
kkB
kB
kQB
k6B
kkB
kQB
lWB
lWB
lWB
lWB
lWB
lWB
lWB
mCB
mCB
mCB
m]B
m]B
mCB
m]B
m]B
mCB
mCB
mCB
nIB
ncB
ncB
oOB
oiB
oiB
o�B
oOB
poB
poB
pUB
poB
poB
pUB
poB
poB
q[111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.62(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         202002010034382020020100343820200201003438202306231720242023062317202420230623172024202002020029512020020200295120200202002951  JA  ARFMdecpA19c                                                                20200127153817  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20200127063821  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20200127063823  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20200127063824  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20200127063825  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20200127063825  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20200127063825  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20200127063825  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20200127063826  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20200127063827                      G�O�G�O�G�O�                JA  ARUP                                                                        20200127065509                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20200127153500  CV  JULD            G�O�G�O�F��I                JM  ARCAJMQC2.0                                                                 20200131153438  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20200131153438  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20200201152951  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20230623082024  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20230705031507                      G�O�G�O�G�O�                