CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2020-02-17T15:38:26Z creation;2020-02-17T15:38:29Z conversion to V3.1;2022-11-21T05:27:32Z update;     
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
_FillValue                    �(Argo profile    3.1 1.2 19500101000000  20200217153826  20221123114511  4902148 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I1_0397_201                     2C  D   NAVIS_A                         0397                            ARGO 011514                     863 @��*�6 1   @����b�@;��u�dxQ��1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!�fD"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�C3D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�C3D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�@ D�3D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @5�@{�@�@�A�HA>�HA^�HA~�HA�p�A�p�A�p�A�p�A�p�A�p�A�p�A�p�B�RB�RB�RB�RB'�RB/�RB7�RB?�RBG�RBO�RBW�RB_�RBg�RBo�RBw�RB�RB��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��=C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
D {�D ��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D	{�D	��D
{�D
��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D {�D ��D!��D!��D"{�D"��D#{�D#��D${�D$��D%{�D%��D&{�D&��D'{�D'��D({�D(��D){�D)��D*{�D*��D+{�D+��D,{�D,��D-{�D-��D.{�D.��D/{�D/��D0{�D0��D1{�D1��D2{�D2��D3{�D3��D4{�D4��D5{�D5��D6{�D6��D7{�D7��D8{�D8��D9{�D9��D:{�D:��D;{�D;��D<{�D<��D={�D=��D>{�D>��D?{�D?��D@{�D@��DA{�DA��DB{�DB��DC{�DC��DD{�DD��DE{�DE��DF{�DF��DG{�DG��DH{�DH��DI{�DI��DJ{�DJ��DK{�DK��DL{�DL��DM{�DM��DN{�DN��DO{�DO��DP{�DP��DQ{�DQ��DR{�DR��DS{�DS��DT{�DT��DU{�DU��DV{�DV��DW{�DW��DX{�DX��DY{�DY��DZ{�DZ��D[{�D[��D\{�D\��D]{�D]��D^{�D^��D_{�D_��D`{�D`��Da{�Da��Db{�Db��Dc{�Dc��Dd{�Dd��De{�De��Df{�Df��Dg{�Dg��Dh{�Dh��Di{�Di��Dj{�Dj��Dk{�Dk��Dl{�Dl��Dm{�Dm��Dn{�Dn��Do{�Do��Dp{�Dp��Dq{�Dq��Dr{�Dr��Ds{�Ds��Dt{�Dt��Du{�Du��Dv{�Dv��Dw{�Dw��Dx{�Dx��Dy{�Dy��Dz{�Dz��D{{�D{��D|{�D|��D}{�D}��D~{�D~��D{�D��D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D� �D�@�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D½�D���D�=�D�}�Dý�D���D�=�D�}�DĽ�D���D�=�D�}�DŽ�D���D�=�D�}�Dƽ�D���D�=�D�}�Dǽ�D���D�=�D�}�DȽ�D���D�=�D�}�Dɽ�D���D�=�D�}�Dʽ�D���D�=�D�}�D˽�D���D�=�D�}�D̽�D���D�=�D�}�Dͽ�D���D�=�D�}�Dν�D���D�=�D�}�DϽ�D���D�=�D�}�Dн�D���D�=�D�}�Dѽ�D���D�=�D�}�Dҽ�D���D�=�D�}�Dӽ�D���D�=�D�}�DԽ�D���D�=�D�}�Dս�D���D�=�D�}�Dֽ�D���D�=�D�}�D׽�D���D�=�D�}�Dؽ�D���D�=�D�}�Dٽ�D���D�=�D�}�Dڽ�D���D�=�D�}�D۽�D���D�=�D�}�Dܽ�D���D�=�D�}�Dݽ�D���D�=�D�}�D޽�D���D�=�D�}�D߽�D���D�=�D�}�D��D���D�=�D�}�D��D���D�@�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D���D���D�=�D�}�D��D���D�=�D��D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D��)1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�ȴA�ȴA���A���A���A�ȴA�ĜA�A�A���A�ȴA��9A�n�A���A�G�A��HA�r�A�r�A��A��A�p�A�O�A�&�A��;A��PA�n�A�5?A�JA��#A��`A��`A�ȴA�G�A��A���A�A��^A��!A���A��A�G�A�  A���A�JA���A�%A���A��/A�+A�A���A�hsA�A�A�?}A�$�A�bA��7A�n�A��A��A�"�A�K�A�hsA��#A��PA�l�A�;dA���A�x�A�/A���A��uA��A�|�A�
=A���A��TA�ffA�A���A�|�A�+A��wA��A���A���A��A��#A��;A��A�ƨA�~�A�E�A��\A�VA���A�VA�r�A�1A�1'A�A��A�G�A��A~��A|ȴAz�Ay��Aw��Aw`BAwAv�DAv$�Au��Au��Ar�HAn�Am
=Ak�FAk��Ak�hAkdZAj�9Ai��Ai�PAiAh^5Ag��Af��Ae��Ad1'AcXAb�9Aa�TAaVA_�FA^��A]VA[��A[7LAZ��AZ  AY�FAY�PAX��AX~�AW�AWt�AV�!AU�AT��ATffAT=qAR��ARz�AR{APjAO"�AM�TAL�uAK�;AJ~�AI�AH��AG��AE�hAB  A?��A?&�A=ƨA<��A<JA;�mA;K�A:ĜA:Q�A9p�A9/A9%A8v�A7�#A6��A5O�A4r�A3�mA3x�A2��A2��A2�A2VA1�7A1/A0�\A0A/�FA/`BA/;dA.Q�A,�!A+��A+7LA*~�A* �A)�wA(��A(jA'�
A'"�A&��A&�RA%�hA"��A"JA!;dA ��A �DA M�A {A�
A�A?}A�A~�A��AȴAz�A�
A+A�-Av�Av�A�`AXA��An�At�A�RA��A5?Ax�AO�A��A~�A�^A
�A	"�AȴAI�A�DA�TA��A`BAG�A��Ar�AVA  At�Av�A�7A ��A �!@��@�~�@���@�  @�ff@���@�X@��/@�Q�@��m@���@�A�@�V@�"�@�ff@�@��#@���@�x�@���@�Q�@�+@�+@�/@�w@�+@�@�l�@��@�v�@�j@ޟ�@�E�@�{@ݲ-@��@�Z@۝�@�V@ف@؃@�dZ@�&�@Ӯ@�E�@мj@�1'@�ƨ@���@�^5@���@�(�@�l�@�ȴ@�=q@��@�V@ǍP@�Z@�n�@�J@���@��@��9@��@���@�
=@�x�@�A�@��@�n�@���@�r�@�33@���@�V@�{@��#@���@��;@�;d@���@��T@��@�7L@��D@��@�C�@�V@�7L@���@�z�@��@�33@��@��R@��@�&�@�dZ@��#@��7@�V@�(�@�1@���@���@���@�t�@�C�@���@�5?@�Q�@�ƨ@�+@���@��@�X@���@���@���@��\@�@���@���@��w@�l�@��@��+@�=q@�-@�$�@��@��@�{@�J@��#@���@��h@�p�@�7L@�Ĝ@���@�z�@�I�@��w@�@���@��7@�`B@���@��@�9X@�ƨ@���@�t�@���@�5?@��@���@���@���@��@�Q�@� �@���@���@���@�+@��!@��+@�$�@��@��T@�@��h@�O�@���@�bN@���@���@�C�@�o@�@�ȴ@�V@��@��^@���@��h@�p�@�O�@�/@���@��@�(�@�1@�;@��@�;@�@�w@
=@~$�@}p�@}`B@}�@|1@{ƨ@{�@{��@|Z@|z�@|�@|9X@|(�@{�
@{�@{��@{o@z-@y�^@yhs@x��@x�@x1'@w�@wl�@vE�@u�h@u`B@u?}@t��@t�j@t�@tz�@t1@st�@r�H@r~�@r-@q��@qX@p��@pĜ@p�u@o�@n��@nV@n5?@m�T@m�-@m?}@l��@l��@m?}@m`B@mp�@mp�@m`B@mO�@m/@m/@l�@l1@k�
@k�F@kdZ@j��@j-@i��@i��@i��@i�^@iG�@hbN@hA�@h1'@g�w@g+@e�-@e/@d��@d��@d��@dj@dZ@dZ@dj@d�D@cdZ@co@b��@b-@a�#@a��@a��@aX@`��@`�@`A�@`b@_��@_|�@^�y@^��@^ff@^E�@^@]?}@\��@]�@]�@\I�@[�m@[33@Z��@Z��@ZJ@Y��@Yx�@Y&�@X�u@Xr�@XbN@X1'@X1'@X �@X  @W��@WK�@V��@Vff@VV@Vff@V$�@U@T��@TZ@T9X@T�@S�
@S��@SC�@R��@R=q@Q��@Qhs@Q�@P��@PbN@Pb@O�;@O�@OK�@O�@N�@Nȴ@N�+@Nff@Nff@NV@M�T@M�@M`B@MO�@M�@L�/@L�j@L�D@Lz�@Lz�@LZ@L�@K��@K�m@K�@J�@J��@J~�@I��@IG�@H�`@H�9@H1'@G|�@G�@F�R@Fff@FV@F5?@F$�@F$�@E�@E�T@E�h@E`B@E?}@E�@D�@D��@D�@Dj@C��@B�H@B�\@B=q@AG�@A&�@A&�@A%@@�@?�@?�w@?|�@?l�@?K�@?
=@?
=@?
=@>�y@>�+@=@=O�@<�/@<z�@<Z@<9X@<�@;�m@;ƨ@;�F@;��@;dZ@;"�@:��@:�\@:n�@:-@:J@:J@9�^@9x�@9G�@9&�@9%@8r�@81'@7�@7�w@7\)@7
=@7
=@6�y@6�+@6V@6{@5��@5/@4�@4��@4Z@4(�@4�@41@3��@3�F@3dZ@2�@2^5@2�@2J@1��@1��@1�7@1x�@17L@0��@0 �@/�@/��@/��@.ȴ@.v�@.v�@-�T@-��@-O�@,�/@,�@,z�@,z�@,z�@,j@,�@+��@+C�@*�H@*�\@*-@)�@)��@)�@(Ĝ@(��@(�@(Q�@(b@'�@'�@'�;@'��@'K�@'�@'
=@&�y@&V@&@%@%p�@%O�@%?}@%/@$��@$�@$�j@$��@$j@$Z@$9X@#��@#ƨ@#�F@#��@#t�@#33@"�@"��@"n�@"=q@!��@!x�@!G�@!&�@!%@ ��@ 1'@  �@  �@  �@   @�@��@l�@ȴ@ff@V@V@$�@@�-@�h@�@p�@O�@?}@�@�/@�@�@��@Z@�@�F@dZ@S�@o@�!@�!@��@�\@�\@n�@=q@J@�@�#@��@X@%@%@�`@��@�9@�@bN@A�@1'@�;@�w@��@\)@
=@ȴ@��@v�@E�@�@��@@��@p�@O�@/@�@��@��@�@�/@�j@��@Z@(�@(�@1@��@�
@ƨ@�@33@�H@��@��@��@~�@n�@^5@M�@-@-@�@��@��@x�@X@G�@&�@��@��@�`@Ĝ@�9@�u@bN@bN@Q�@  @�@��@l�@;d@;d@+@�@��@5?@�T@��@�h@�@p�@`B@V@��@�@�@�@�@(�@�
@��@dZ@S�@33@"�@
�H@
��@
^5@
=q@
�@	�#@	��@	��@	��@	�7@	x�@	�@	�@	%@��@�@ �@  @�@�;@�;@��@��@��@�w@�P@l�@K�@�@�y@�+@�+@V@$�@�@��@@��@O�@O�@?}@�@�j@�@��@j@I�@�@�m@�
@ƨ@�@33@o@�@�!@^51111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�ȴA�ȴA���A���A���A�ȴA�ĜA�A�A���A�ȴA��9A�n�A���A�G�A��HA�r�A�r�A��A��A�p�A�O�A�&�A��;A��PA�n�A�5?A�JA��#A��`A��`A�ȴA�G�A��A���A�A��^A��!A���A��A�G�A�  A���A�JA���A�%A���A��/A�+A�A���A�hsA�A�A�?}A�$�A�bA��7A�n�A��A��A�"�A�K�A�hsA��#A��PA�l�A�;dA���A�x�A�/A���A��uA��A�|�A�
=A���A��TA�ffA�A���A�|�A�+A��wA��A���A���A��A��#A��;A��A�ƨA�~�A�E�A��\A�VA���A�VA�r�A�1A�1'A�A��A�G�A��A~��A|ȴAz�Ay��Aw��Aw`BAwAv�DAv$�Au��Au��Ar�HAn�Am
=Ak�FAk��Ak�hAkdZAj�9Ai��Ai�PAiAh^5Ag��Af��Ae��Ad1'AcXAb�9Aa�TAaVA_�FA^��A]VA[��A[7LAZ��AZ  AY�FAY�PAX��AX~�AW�AWt�AV�!AU�AT��ATffAT=qAR��ARz�AR{APjAO"�AM�TAL�uAK�;AJ~�AI�AH��AG��AE�hAB  A?��A?&�A=ƨA<��A<JA;�mA;K�A:ĜA:Q�A9p�A9/A9%A8v�A7�#A6��A5O�A4r�A3�mA3x�A2��A2��A2�A2VA1�7A1/A0�\A0A/�FA/`BA/;dA.Q�A,�!A+��A+7LA*~�A* �A)�wA(��A(jA'�
A'"�A&��A&�RA%�hA"��A"JA!;dA ��A �DA M�A {A�
A�A?}A�A~�A��AȴAz�A�
A+A�-Av�Av�A�`AXA��An�At�A�RA��A5?Ax�AO�A��A~�A�^A
�A	"�AȴAI�A�DA�TA��A`BAG�A��Ar�AVA  At�Av�A�7A ��A �!@��@�~�@���@�  @�ff@���@�X@��/@�Q�@��m@���@�A�@�V@�"�@�ff@�@��#@���@�x�@���@�Q�@�+@�+@�/@�w@�+@�@�l�@��@�v�@�j@ޟ�@�E�@�{@ݲ-@��@�Z@۝�@�V@ف@؃@�dZ@�&�@Ӯ@�E�@мj@�1'@�ƨ@���@�^5@���@�(�@�l�@�ȴ@�=q@��@�V@ǍP@�Z@�n�@�J@���@��@��9@��@���@�
=@�x�@�A�@��@�n�@���@�r�@�33@���@�V@�{@��#@���@��;@�;d@���@��T@��@�7L@��D@��@�C�@�V@�7L@���@�z�@��@�33@��@��R@��@�&�@�dZ@��#@��7@�V@�(�@�1@���@���@���@�t�@�C�@���@�5?@�Q�@�ƨ@�+@���@��@�X@���@���@���@��\@�@���@���@��w@�l�@��@��+@�=q@�-@�$�@��@��@�{@�J@��#@���@��h@�p�@�7L@�Ĝ@���@�z�@�I�@��w@�@���@��7@�`B@���@��@�9X@�ƨ@���@�t�@���@�5?@��@���@���@���@��@�Q�@� �@���@���@���@�+@��!@��+@�$�@��@��T@�@��h@�O�@���@�bN@���@���@�C�@�o@�@�ȴ@�V@��@��^@���@��h@�p�@�O�@�/@���@��@�(�@�1@�;@��@�;@�@�w@
=@~$�@}p�@}`B@}�@|1@{ƨ@{�@{��@|Z@|z�@|�@|9X@|(�@{�
@{�@{��@{o@z-@y�^@yhs@x��@x�@x1'@w�@wl�@vE�@u�h@u`B@u?}@t��@t�j@t�@tz�@t1@st�@r�H@r~�@r-@q��@qX@p��@pĜ@p�u@o�@n��@nV@n5?@m�T@m�-@m?}@l��@l��@m?}@m`B@mp�@mp�@m`B@mO�@m/@m/@l�@l1@k�
@k�F@kdZ@j��@j-@i��@i��@i��@i�^@iG�@hbN@hA�@h1'@g�w@g+@e�-@e/@d��@d��@d��@dj@dZ@dZ@dj@d�D@cdZ@co@b��@b-@a�#@a��@a��@aX@`��@`�@`A�@`b@_��@_|�@^�y@^��@^ff@^E�@^@]?}@\��@]�@]�@\I�@[�m@[33@Z��@Z��@ZJ@Y��@Yx�@Y&�@X�u@Xr�@XbN@X1'@X1'@X �@X  @W��@WK�@V��@Vff@VV@Vff@V$�@U@T��@TZ@T9X@T�@S�
@S��@SC�@R��@R=q@Q��@Qhs@Q�@P��@PbN@Pb@O�;@O�@OK�@O�@N�@Nȴ@N�+@Nff@Nff@NV@M�T@M�@M`B@MO�@M�@L�/@L�j@L�D@Lz�@Lz�@LZ@L�@K��@K�m@K�@J�@J��@J~�@I��@IG�@H�`@H�9@H1'@G|�@G�@F�R@Fff@FV@F5?@F$�@F$�@E�@E�T@E�h@E`B@E?}@E�@D�@D��@D�@Dj@C��@B�H@B�\@B=q@AG�@A&�@A&�@A%@@�@?�@?�w@?|�@?l�@?K�@?
=@?
=@?
=@>�y@>�+@=@=O�@<�/@<z�@<Z@<9X@<�@;�m@;ƨ@;�F@;��@;dZ@;"�@:��@:�\@:n�@:-@:J@:J@9�^@9x�@9G�@9&�@9%@8r�@81'@7�@7�w@7\)@7
=@7
=@6�y@6�+@6V@6{@5��@5/@4�@4��@4Z@4(�@4�@41@3��@3�F@3dZ@2�@2^5@2�@2J@1��@1��@1�7@1x�@17L@0��@0 �@/�@/��@/��@.ȴ@.v�@.v�@-�T@-��@-O�@,�/@,�@,z�@,z�@,z�@,j@,�@+��@+C�@*�H@*�\@*-@)�@)��@)�@(Ĝ@(��@(�@(Q�@(b@'�@'�@'�;@'��@'K�@'�@'
=@&�y@&V@&@%@%p�@%O�@%?}@%/@$��@$�@$�j@$��@$j@$Z@$9X@#��@#ƨ@#�F@#��@#t�@#33@"�@"��@"n�@"=q@!��@!x�@!G�@!&�@!%@ ��@ 1'@  �@  �@  �@   @�@��@l�@ȴ@ff@V@V@$�@@�-@�h@�@p�@O�@?}@�@�/@�@�@��@Z@�@�F@dZ@S�@o@�!@�!@��@�\@�\@n�@=q@J@�@�#@��@X@%@%@�`@��@�9@�@bN@A�@1'@�;@�w@��@\)@
=@ȴ@��@v�@E�@�@��@@��@p�@O�@/@�@��@��@�@�/@�j@��@Z@(�@(�@1@��@�
@ƨ@�@33@�H@��@��@��@~�@n�@^5@M�@-@-@�@��@��@x�@X@G�@&�@��@��@�`@Ĝ@�9@�u@bN@bN@Q�@  @�@��@l�@;d@;d@+@�@��@5?@�T@��@�h@�@p�@`B@V@��@�@�@�@�@(�@�
@��@dZ@S�@33@"�@
�H@
��@
^5@
=q@
�@	�#@	��@	��@	��@	�7@	x�@	�@	�@	%@��@�@ �@  @�@�;@�;@��@��@��@�w@�P@l�@K�@�@�y@�+@�+@V@$�@�@��@@��@O�@O�@?}@�@�j@�@��@j@I�@�@�m@�
@ƨ@�@33@o@�@�!@^51111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�+B�+B�+B�+B�+B�+B�+B�+B�+B�+B�+B�+B�1B�PB�bB�{B��B��B��B�uB�1B�B� B� B�JB�oB�uB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B�bB�7Bw�BZB@�B>wB5?B0!B/B-B)�B&�B&�B$�B"�B�B�B�B�BVB%B��B��B��B��B��B�B�mB�NB�#B�
B��B��BȴB�B��B�=B�B�B~�Bw�B]/BS�BN�B<jB#�B�BJB
��B
��B
�B
�B
�TB
�)B
�B
��B
�LB
�-B
��B
��B
�bB
�B
z�B
r�B
bNB
S�B
K�B
A�B
>wB
<jB
9XB
6FB
33B
.B
�B
%B	��B	�B	�B	�B	�B	�B	�mB	�fB	�`B	�NB	�BB	�/B	�B	��B	��B	ǮB	B	�jB	�?B	�'B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�{B	�\B	�=B	�%B	�B	�B	{�B	x�B	u�B	k�B	cTB	^5B	VB	P�B	G�B	A�B	=qB	5?B	)�B	�B	oB	bB	VB	
=B	1B	%B	B	B	B	  B��B��B��B�B�B�ZB�BB�/B�#B�B�B�
B�B��B��B��B��B��BɺBǮBĜB�wB�dB�XB�FB�9B�3B�!B�B�B��B��B��B��B��B��B�uB�hB�bB�\B�\B�VB�JB�DB�7B�%B~�B}�B{�Bz�Bv�Bs�Bn�BiyBe`BbNB`BB_;B]/B[#BXBVBT�BS�BQ�BO�BL�BJ�BH�BF�BE�BA�B@�B@�B?}B?}B>wB=qB<jB<jB;dB9XB8RB7LB6FB49B33B2-B2-B1'B1'B0!B0!B/B.B-B)�B(�B'�B'�B'�B'�B&�B&�B&�B%�B%�B$�B$�B#�B#�B#�B"�B"�B!�B!�B"�B!�B"�B!�B!�B!�B �B!�B!�B!�B!�B"�B"�B#�B$�B#�B#�B#�B#�B$�B%�B&�B&�B&�B&�B&�B&�B)�B-B-B-B.B-B.B1'B5?B7LB9XB:^B<jB=qB?}BA�BB�BC�BC�BC�BE�BH�BI�BK�BM�BN�BO�BQ�BR�BR�BYB\)B]/B^5B_;BcTBdZBcTBe`BffBk�Bo�Bp�Bq�Bu�Bv�Bv�Bw�Bx�By�Bz�B{�B|�B�B�B�%B�7B�=B�PB�\B��B��B��B��B��B��B��B�B�B�!B�-B�-B�-B�3B�3B�3B�3B�9B�?B�?B�FB�LB�^B�dB�jB�qB��BŢB��B��B��B��B��B�
B�B�B�#B�BB�NB�ZB�fB�B�B�B�B�B��B��B��B��B��B��B	B	B	B	B	%B	1B	PB	hB	{B	�B	�B	�B	�B	�B	!�B	%�B	'�B	'�B	(�B	+B	,B	-B	/B	2-B	6FB	9XB	<jB	=qB	?}B	B�B	B�B	C�B	E�B	F�B	H�B	L�B	R�B	S�B	T�B	VB	[#B	\)B	]/B	^5B	^5B	^5B	`BB	cTB	dZB	gmB	iyB	l�B	n�B	o�B	o�B	o�B	p�B	r�B	v�B	y�B	z�B	z�B	{�B	|�B	}�B	~�B	�B	�B	�B	�B	�%B	�+B	�7B	�=B	�DB	�oB	�uB	�uB	�uB	�uB	�uB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�!B	�'B	�'B	�!B	�'B	�3B	�FB	�XB	�^B	�dB	�dB	�dB	�jB	�wB	��B	B	ÖB	ŢB	ǮB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�#B	�#B	�/B	�/B	�/B	�5B	�;B	�;B	�BB	�HB	�HB	�NB	�NB	�NB	�TB	�ZB	�ZB	�ZB	�ZB	�`B	�`B	�mB	�sB	�sB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
%B
%B
+B
+B
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
PB
\B
bB
hB
uB
uB
uB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
"�B
"�B
"�B
#�B
#�B
#�B
$�B
%�B
%�B
%�B
%�B
'�B
'�B
'�B
(�B
)�B
)�B
)�B
+B
-B
-B
-B
-B
.B
/B
/B
/B
/B
/B
/B
/B
0!B
0!B
1'B
2-B
33B
33B
33B
33B
49B
33B
49B
5?B
6FB
7LB
7LB
7LB
9XB
9XB
9XB
9XB
:^B
:^B
;dB
;dB
<jB
<jB
<jB
<jB
<jB
=qB
=qB
>wB
>wB
?}B
?}B
?}B
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
H�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
K�B
L�B
L�B
L�B
L�B
M�B
M�B
N�B
M�B
M�B
N�B
N�B
N�B
N�B
O�B
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
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
aHB
aHB
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
cTB
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
jB
jB
k�B
k�B
l�B
l�B
l�B
m�B
m�B
n�B
n�B
n�B
n�B
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
p�B
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
r�B
s�B
s�B
s�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
v�B
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
y�B
y�B
y�B
y�B
z�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�EB�+B�+B�+B�+B�+B�+B�+B�+B�EB��B�1B��B�VB��B��B�+B��B��B��B��B��B��B��B��B��B��B��B��B��B�BB��B�VB��B��B��B��B��B�B�B�SB��B��BzB[�BA�B@iB6�B0�B/�B-wB*KB'B'8B%zB$tB�B�B�B�BbB�B�"B�zB�JB�jB��B�iB�$B�B��B�YBՁB�{B˒B�UB��B��B��B��B�iBz�B_BU�BQ�B@ B&�BeBvB
��B
�tB
�B
�/B
�B
ݲB
�KB
�-B
��B
��B
��B
��B
�:B
�{B
|PB
u%B
dtB
U�B
M�B
B'B
>�B
=B
9�B
6�B
4�B
1�B
"�B
1B	�0B	��B	��B	�'B	��B	�qB	�$B	�RB	�LB	�nB	�bB	�jB	�B	��B	��B	��B	��B	�B	��B	��B	�WB	��B	�\B	�~B	�B	��B	�QB	�QB	�eB	�sB	��B	��B	��B	��B	��B	�{B	|�B	y�B	w�B	mCB	d�B	_�B	W?B	R�B	IRB	B�B	?cB	8RB	-�B	�B	�B	:B	�B	B	�B	�B	�B	�B	B	 iB�wB��B�B�3B�)B�zB��B��B��B�B�yBרB��BөBѷBϑB�PB�JB�rB�BƎB��B�6B�^B��B��B�TB�B��B��B��B��B��B��B��B��B�B��B��B��B��B��B��B��B�#B��B}B~�B|�B|6Bx�Bu�Bq'Bk�Bg8BcTB`�B`�B^jB\�BY�BV�BUgBT�BR�BQNBN�BK�BIlBG�BG�BB�BABAB?�B@4B>�B=�B="B=�B<�B:�B9$B8B7fB5ZB4B3MB33B1�B1vB0�B0�B/�B/5B/ B+�B*B(sB(>B($B(>B'8B'�B'�B&�B&�B%�B&B$�B%,B$�B#TB#�B# B#B# B"4B#TB"hB"hB"�B!�B"�B"�B"�B#nB#�B#�B$�B%`B$ZB$tB$tB$�B%zB&�B'mB'�B'mB'�B(sB(�B+B-]B-wB-�B.�B.cB/�B2�B6`B8B9�B;dB=<B>]B@OBBBCBC�BC�BD�BFYBI7BJXBLJBN<BOBBP�BR�BS�BS�BY�B\�B]�B^�B_�Bc�Bd�BdBf2Bg�Bl�Bo�BqABrGBu�Bv�BwBxBy	Bz*B{JB|�B~(B��B��B��B��B��B��B�.B�B��B�kB�jB�TB�8B�_B�kB�cB�UB�aB�aB�GB�hB�MB�MB�hB�nB�tB��B��B��B��B��B��B��B� B�YB�B�(B�bB�oB�gB�YB�QB�kB��B�B�B�B�B��B��B��B��B��B��B�B�+B�>B�(B�HB	[B	-B	MB	SB	�B	�B	�B	�B	�B	�B	�B	�B	�B	B	"4B	&B	(
B	(
B	)*B	+B	,=B	-]B	/iB	2�B	6`B	9rB	<�B	=�B	?�B	B�B	B�B	C�B	E�B	F�B	IB	M6B	S&B	T,B	T�B	U�B	[#B	\CB	]dB	^jB	^jB	^jB	`\B	c�B	d�B	g�B	i�B	l�B	n�B	o�B	o�B	o�B	q'B	sB	v�B	zB	z�B	z�B	|B	}B	~(B	HB	�;B	�AB	�GB	�MB	�YB	�_B	�RB	�rB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	��B	��B	�&B	�,B	�B	�B	��B	�"B	�"B	�CB	�}B	�UB	�UB	�vB	��B	��B	�[B	�MB	�FB	�rB	�xB	�B	�dB	�dB	��B	��B	��B	��B	��B	żB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�4B	�B	�B	�&B	�,B	�FB	�9B	�B	�+B	�B	�QB	�QB	�QB	�=B	�WB	�IB	�IB	�dB	�jB	�pB	�pB	�vB	�HB	�|B	�B	�B	�B	�B	�tB	�ZB	�ZB	�B	�B	��B	�B	�B	�B	�B	�B	��B	��B	�B	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�	B	��B	�B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�"B	�"B	�"B	�B	�.B	�cB
 OB
;B
'B
GB
aB
SB
SB
YB
?B
EB
+B
EB
EB
KB
KB
	lB
	RB
	lB

XB

XB
^B
xB
�B
�B
�B
�B
�B
uB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
"�B
"�B
"�B
#�B
$B
$B
$�B
%�B
%�B
%�B
&2B
(
B
(
B
($B
)*B
*B
)�B
*0B
+6B
-)B
-)B
-]B
-]B
./B
/OB
/5B
/5B
/5B
/5B
/OB
/5B
0UB
0UB
1vB
2aB
3MB
3hB
3MB
3MB
4TB
3hB
4�B
5�B
6`B
7fB
7fB
7�B
9rB
9rB
9�B
9�B
:xB
:�B
;B
;B
<�B
<jB
<�B
<�B
<�B
=�B
=�B
>�B
>�B
?�B
?�B
?�B
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
H�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
KB
K�B
L�B
L�B
MB
MB
NB
M�B
N�B
M�B
NB
OB
N�B
OB
O(B
O�B
P�B
Q B
QB
QB
RB
RB
Q�B
RB
RB
R�B
S&B
SB
S&B
TB
TB
T,B
T,B
T,B
UB
U2B
UB
V9B
VB
VB
W
B
W
B
W$B
W$B
W$B
W$B
X+B
X+B
X+B
Y1B
Y1B
Y1B
Y1B
Y1B
YKB
Z7B
ZQB
ZQB
Z7B
[WB
[WB
[=B
[WB
\CB
\]B
]dB
]dB
]dB
]IB
^5B
^OB
^OB
^OB
^OB
_VB
_VB
_;B
_;B
_VB
_VB
_pB
`vB
`\B
`BB
`\B
`\B
`\B
`\B
`\B
abB
abB
bhB
bhB
bhB
b�B
bNB
cTB
cnB
cnB
cnB
c�B
cnB
cnB
d�B
dtB
dtB
ezB
e�B
ezB
ezB
ezB
ezB
ezB
f�B
ffB
f�B
f�B
f�B
f�B
f�B
g�B
gmB
g�B
g�B
g�B
h�B
h�B
i�B
i�B
iyB
iyB
i�B
j�B
j�B
jB
j�B
j�B
k�B
k�B
l�B
l�B
l�B
m�B
m�B
n�B
n�B
n�B
n�B
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
p�B
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
r�B
s�B
s�B
s�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
xB
xB
w�B
x�B
x�B
y	B
x�B
x�B
y�B
zB
y�B
y�B
z�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.07(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         202002290033482020022900334820200229003348202211182142052022111821420520221118214205202003010016572020030100165720200301001657  JA  ARFMdecpA19c                                                                20200218003745  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20200217153826  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20200217153828  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20200217153828  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20200217153829  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20200217153829  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20200217153829  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcp2.8e                                                                20200217153829  QCF$                G�O�G�O�G�O�            8000JA  ARGQaqcp2.8e                                                                20200217153829  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20200217153829  QCF$                G�O�G�O�G�O�            8000JA  ARGQrqcpt16c                                                                20200217153829  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20200217153829                      G�O�G�O�G�O�                JA  ARUP                                                                        20200217155357                      G�O�G�O�G�O�                JM  ARGQrqcjv291                                                                20200217153313  QCP$                G�O�G�O�G�O�2DEB7C          JM  ARGQJMQC2.0                                                                 20200217153247  CV  JULD            G�O�G�O�F��                JM  ARCAJMQC2.0                                                                 20200228153348  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20200228153348  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20200229151657  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221118124205  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221123114511                      G�O�G�O�G�O�                