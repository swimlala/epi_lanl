CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       ~2018-11-24T15:37:11Z creation;2018-11-24T15:37:14Z conversion to V3.1;2019-12-18T07:18:42Z update;2022-11-21T05:29:48Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         BPRIMARY|https://orcid.org/0000-0001-9150-6442|Kanako Sato, JAMSTEC        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7H   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7X   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7\   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7`   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7p   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  8   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8H   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8L   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8P   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     8T   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8t   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8x   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8|   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
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
_FillValue                    9   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    9   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        :   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  :    PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]@   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  a(   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �H   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �P   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �8   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �p   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ޜ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �    HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �$   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �(   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �,   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �l   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �|   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181124153711  20221123111508  4902148 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I1_0397_156                     2C  Dd!�NAVIS_A                         0397                            ARGO 011514                     863 @ؓ"�K� 1   @ؓ#��À@<7�4m���d!����D1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�33A   AffA@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&�fD'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111)@�(�@���@�AG�A>�HA^�HA~�HA�p�A�p�A�p�A�p�A�p�A�p�A�p�A�p�B�RB�RB�RB�RB'�RB/�RB7�RB?�RBG�RBO�RBW�RB_�RBg�RBo�RBw�RB�RB��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��
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
D {�D ��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D	{�D	��D
{�D
��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D {�D ��D!{�D!��D"{�D"��D#{�D#��D${�D$��D%{�D%��D&��D&��D'{�D'��D({�D(��D){�D)��D*{�D*��D+{�D+��D,{�D,��D-{�D-��D.{�D.��D/{�D/��D0{�D0��D1{�D1��D2{�D2��D3{�D3��D4{�D4��D5{�D5��D6{�D6��D7{�D7��D8{�D8��D9{�D9��D:{�D:��D;{�D;��D<{�D<��D={�D=��D>{�D>��D?{�D?��D@{�D@��DA{�DA��DB{�DB��DC{�DC��DD{�DD��DE{�DE��DF{�DF��DG{�DG��DH{�DH��DI{�DI��DJ{�DJ��DK{�DK��DL{�DL��DM{�DM��DN{�DN��DO{�DO��DP{�DP��DQ{�DQ��DR{�DR��DS{�DS��DT{�DT��DU{�DU��DV{�DV��DW{�DW��DX{�DX��DY{�DY��DZ{�DZ��D[{�D[��D\{�D\��D]{�D]��D^{�D^��D_{�D_��D`{�D`��Da{�Da��Db{�Db��Dc{�Dc��Dd{�Dd��De{�De��Df{�Df��Dg{�Dg��Dh{�Dh��Di{�Di��Dj{�Dj��Dk{�Dk��Dl{�Dl��Dm{�Dm��Dn{�Dn��Do{�Do��Dp{�Dp��Dq{�Dq��Dr{�Dr��Ds{�Ds��Dt{�Dt��Du{�Du��Dv{�Dv��Dw{�Dw��Dx{�Dx��Dy{�Dy��Dz{�Dz��D{{�D{��D|{�D|��D}{�D}��D~{�D~��D{�D��D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D½�D���D�=�D�}�Dý�D���D�=�D�}�DĽ�D���D�=�D�}�DŽ�D���D�=�D�}�Dƽ�D���D�=�D�}�Dǽ�D���D�=�D�}�DȽ�D���D�=�D�}�Dɽ�D���D�=�D�}�Dʽ�D���D�=�D�}�D˽�D���D�=�D�}�D̽�D���D�=�D�}�Dͽ�D���D�=�D�}�Dν�D���D�=�D�}�DϽ�D���D�=�D�}�Dн�D���D�=�D�}�Dѽ�D���D�=�D�}�Dҽ�D���D�=�D�}�Dӽ�D���D�=�D�}�DԽ�D���D�=�D�}�Dս�D���D�=�D�}�Dֽ�D���D�=�D�}�D׽�D���D�=�D�}�Dؽ�D���D�=�D�}�Dٽ�D���D�=�D�}�Dڽ�D���D�=�D�}�D۽�D���D�=�D�}�Dܽ�D���D�=�D�}�Dݽ�D���D�=�D�}�D޽�D���D�=�D�}�D߽�D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D���D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�A���A���A���A���A���A�|�A�dZA�`BA�`BA�\)A�VA�O�A�G�A�?}A�-A�$�A� �A��A�"�A��RA�^5A�\)A�ZA�ZA�XA�XA�Q�A�M�A�I�A�A�A�5?A�A�A�I�A��^A���A��RA��A�oA��A�;dA��A��#A�33A���A�bNA���A���A�XA�;dA���A�\)A�ZA�?}A� �A���A�9XA�hsA��HA��A�p�A���A��jA�O�A��yA��A�7LA��A�jA�~�A�$�A���A�
=A�ȴA�dZA�p�A�/A���A�  A���A�&�A���A���A�`BA� �A���A�dZA�v�A���A`BA~bA}`BA{+Ay��AxA�AwXAul�AtZAr1Ap��Ao��Am�^Al�RAkp�Aj�Ah-Ag��AgdZAg"�Ac�
Aa�hA`$�A_A]��A[ƨAZ �AYt�AY��AYG�AX�AV�\AVbAS�AR�AP��AO��ANVAM�#AMl�AL5?AJffAI��AH��AHZAG�^AG7LAF^5AEƨAD��AC�
AA��A@��A@ffA?|�A>z�A=�TA=%A<$�A:�/A:�A9��A9��A9��A9S�A9
=A8��A8E�A7XA6�+A6bNA6  A5VA4��A45?A3��A3\)A2�jA2 �A1G�A0�A/�A/�A/?}A.�A-?}A,��A+�TA+C�A*�RA*  A)�^A)%A(�!A(A�A'hsA&��A&bA${A#�A"jA"bA!�mA!�FA �/A �jA �\A��A�jAQ�A�;A��A�A�AXA�/A=qA�A"�A�!A$�A�A��AO�A�`A-A��A�A��Az�A��Av�A�;AA��A�hA?}A
ffA	`BA��A��A�`A�+AAx�A=qA1'A��A�A ^5@��@�ff@�`B@�5?@���@�P@��@��`@ꟾ@�u@�"�@���@��`@�j@�"�@��@�j@�l�@�M�@�O�@��m@�\)@�X@�l�@҇+@��@�b@϶F@�|�@�|�@υ@�l�@�S�@�S�@�o@�ȴ@ΰ!@Χ�@�~�@�V@���@͑h@�?}@��/@�M�@�\)@�ff@��T@Ł@ŉ7@���@�b@�
=@°!@�5?@��#@��^@��-@���@�x�@�&�@��9@���@��@�M�@�X@�%@�z�@�E�@��7@��@�K�@�"�@��H@��+@�^5@�-@�J@��^@�%@��@�ff@��@�ƨ@���@��\@�^5@��h@�/@���@��@��F@�S�@���@�n�@��@���@�bN@��R@�Z@�I�@�I�@�1'@��R@�^5@�{@�@�V@�bN@��@��!@�J@�p�@��@��@��y@�J@��@��@��-@�?}@��D@��@���@�K�@�@��!@��@�?}@�%@���@���@��@���@��w@���@�S�@��!@�7L@��`@��9@��D@��@�Q�@�(�@��;@�dZ@�33@�"�@�ȴ@��\@��@���@�hs@��@��@��9@��@��@��F@��@��\@�J@��@�G�@�V@���@��F@�C�@�+@��@�@��y@���@�@��@��-@��7@�p�@�O�@��@��u@�I�@��@�;@l�@;d@;d@;d@;d@;d@\)@l�@|�@l�@\)@;d@�@~ȴ@~v�@~$�@}�@}�-@}`B@}O�@}O�@}/@|��@|��@|��@|Z@|1@{�m@{ƨ@{��@{�@{dZ@{S�@z�!@z=q@y��@y�#@y�^@y��@yx�@y7L@x�`@x��@xQ�@x1'@w�;@w�w@w�@w�P@v�y@v��@v{@u��@u�@uO�@t��@t��@tz�@tz�@tZ@tI�@s�
@s��@s��@st�@s"�@r��@r�\@rM�@q��@q�#@q��@qx�@q7L@p��@pĜ@p  @oK�@o
=@n��@n��@n�y@n�y@n�@n�y@n�y@n��@n@m�T@m@m�@m?}@m�@l��@l�/@l�D@k�
@k@j-@ihs@i�@h��@hĜ@h�9@h�9@h��@h�u@hbN@g�;@g�w@g;d@g
=@f�y@f��@e�@e�@e/@d�j@d(�@c�m@c�m@c�F@cdZ@b�\@b-@a�#@a��@a�@`��@`�@`�@`�@_�;@^ȴ@^E�@]O�@\�D@[ƨ@["�@Zn�@YG�@X�`@X�u@W�;@W|�@W�@V��@Vff@VV@VE�@V5?@U��@T�j@T9X@S��@S�
@S��@S��@S�@St�@SS�@SC�@S33@S"�@So@So@R�@R�H@R�!@R~�@Rn�@Rn�@R=q@Q�^@Qx�@QX@QG�@Q%@PĜ@P�9@PA�@O�@O�P@Ol�@O\)@O\)@O\)@O;d@N��@N��@Nff@N$�@M��@M�@M`B@M/@L��@LI�@K��@Ko@J�!@JM�@I��@I�#@I��@I�^@I��@Ihs@IG�@I&�@H�`@HQ�@G��@Gl�@F�y@FV@F$�@E@EO�@D�@D�j@D�D@D(�@C�@C33@C"�@B�H@B�\@BJ@A�^@Ahs@AG�@AG�@@�@?K�@?
=@>��@>{@=�h@=O�@=V@<��@<�D@<Z@<�@;�m@;ƨ@;"�@;o@;@:�@:�@:�H@:�@:�@:�@;@;@;@:�H@:�!@:�\@:n�@:M�@:-@9�^@9X@9&�@9%@8��@9%@8�`@8�@8  @7K�@6��@6��@65?@5p�@5�@4��@4j@4(�@41@3�m@3�m@3�m@3�
@3��@2��@2~�@2M�@1��@1%@0��@0��@0��@0��@0Ĝ@0�@/�;@/�@/|�@.�@.��@.ff@.V@.$�@-@-p�@,��@,�/@,�/@,�/@,�/@,�@,j@,j@,I�@,Z@,9X@+�m@+�F@+��@+dZ@+C�@+o@*^5@*-@*�@)�@)��@)��@)�7@)X@)X@)7L@(��@(Ĝ@(�9@(�u@(r�@(r�@(bN@(Q�@(A�@(A�@( �@'�w@'|�@'K�@'+@'
=@&�R@&ff@%�h@%�@$��@$�j@$�D@#��@#�@#t�@#dZ@#S�@#C�@#"�@#o@"�H@"�\@"~�@"^5@"-@"J@!��@!��@!�@!G�@ A�@��@;d@
=@�+@v�@ff@5?@@��@?}@��@��@z�@I�@9X@(�@�m@�F@��@�@33@��@��@n�@=q@-@J@��@��@��@x�@hs@�`@�9@��@�u@�@�@�@r�@r�@Q�@A�@b@�@�@��@ȴ@�R@��@�+@ff@{@@O�@��@�j@�D@9X@�@��@��@dZ@33@@�@�@��@�!@�!@��@��@�\@=q@J@�@�#@��@��@�7@x�@G�@G�@7L@7L@&�@��@��@��@r�@  @  @�@�@��@;d@�@@@�-@�-@�-@�-@�h@�@O�@��@�/@�@9X@��@ƨ@��@�@t�@S�@C�@C�@33@"�@@
��@
��@
�!@
^5@
M�@
�@	��@	�@	X@��@�u@�@bN@  @��@�w@�@�@��@��@�P@�P@|�@\)@�@�@��@��@V@E�@5?@�@V@�@V@��@�@�@�/@��@�D@9X@�@��@�m@�
@��@�@t�@dZ@dZ@S�@33@o@�!@n�@^5@=q@-@J@�@�^@�7@G�@%@ �`@ �`@ Ĝ@   ?�;d?���?��?��?��?���?�V111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�A���A���A���A���A���A�|�A�dZA�`BA�`BA�\)A�VA�O�A�G�A�?}A�-A�$�A� �A��A�"�A��RA�^5A�\)A�ZA�ZA�XA�XA�Q�A�M�A�I�A�A�A�5?A�A�A�I�A��^A���A��RA��A�oA��A�;dA��A��#A�33A���A�bNA���A���A�XA�;dA���A�\)A�ZA�?}A� �A���A�9XA�hsA��HA��A�p�A���A��jA�O�A��yA��A�7LA��A�jA�~�A�$�A���A�
=A�ȴA�dZA�p�A�/A���A�  A���A�&�A���A���A�`BA� �A���A�dZA�v�A���A`BA~bA}`BA{+Ay��AxA�AwXAul�AtZAr1Ap��Ao��Am�^Al�RAkp�Aj�Ah-Ag��AgdZAg"�Ac�
Aa�hA`$�A_A]��A[ƨAZ �AYt�AY��AYG�AX�AV�\AVbAS�AR�AP��AO��ANVAM�#AMl�AL5?AJffAI��AH��AHZAG�^AG7LAF^5AEƨAD��AC�
AA��A@��A@ffA?|�A>z�A=�TA=%A<$�A:�/A:�A9��A9��A9��A9S�A9
=A8��A8E�A7XA6�+A6bNA6  A5VA4��A45?A3��A3\)A2�jA2 �A1G�A0�A/�A/�A/?}A.�A-?}A,��A+�TA+C�A*�RA*  A)�^A)%A(�!A(A�A'hsA&��A&bA${A#�A"jA"bA!�mA!�FA �/A �jA �\A��A�jAQ�A�;A��A�A�AXA�/A=qA�A"�A�!A$�A�A��AO�A�`A-A��A�A��Az�A��Av�A�;AA��A�hA?}A
ffA	`BA��A��A�`A�+AAx�A=qA1'A��A�A ^5@��@�ff@�`B@�5?@���@�P@��@��`@ꟾ@�u@�"�@���@��`@�j@�"�@��@�j@�l�@�M�@�O�@��m@�\)@�X@�l�@҇+@��@�b@϶F@�|�@�|�@υ@�l�@�S�@�S�@�o@�ȴ@ΰ!@Χ�@�~�@�V@���@͑h@�?}@��/@�M�@�\)@�ff@��T@Ł@ŉ7@���@�b@�
=@°!@�5?@��#@��^@��-@���@�x�@�&�@��9@���@��@�M�@�X@�%@�z�@�E�@��7@��@�K�@�"�@��H@��+@�^5@�-@�J@��^@�%@��@�ff@��@�ƨ@���@��\@�^5@��h@�/@���@��@��F@�S�@���@�n�@��@���@�bN@��R@�Z@�I�@�I�@�1'@��R@�^5@�{@�@�V@�bN@��@��!@�J@�p�@��@��@��y@�J@��@��@��-@�?}@��D@��@���@�K�@�@��!@��@�?}@�%@���@���@��@���@��w@���@�S�@��!@�7L@��`@��9@��D@��@�Q�@�(�@��;@�dZ@�33@�"�@�ȴ@��\@��@���@�hs@��@��@��9@��@��@��F@��@��\@�J@��@�G�@�V@���@��F@�C�@�+@��@�@��y@���@�@��@��-@��7@�p�@�O�@��@��u@�I�@��@�;@l�@;d@;d@;d@;d@;d@\)@l�@|�@l�@\)@;d@�@~ȴ@~v�@~$�@}�@}�-@}`B@}O�@}O�@}/@|��@|��@|��@|Z@|1@{�m@{ƨ@{��@{�@{dZ@{S�@z�!@z=q@y��@y�#@y�^@y��@yx�@y7L@x�`@x��@xQ�@x1'@w�;@w�w@w�@w�P@v�y@v��@v{@u��@u�@uO�@t��@t��@tz�@tz�@tZ@tI�@s�
@s��@s��@st�@s"�@r��@r�\@rM�@q��@q�#@q��@qx�@q7L@p��@pĜ@p  @oK�@o
=@n��@n��@n�y@n�y@n�@n�y@n�y@n��@n@m�T@m@m�@m?}@m�@l��@l�/@l�D@k�
@k@j-@ihs@i�@h��@hĜ@h�9@h�9@h��@h�u@hbN@g�;@g�w@g;d@g
=@f�y@f��@e�@e�@e/@d�j@d(�@c�m@c�m@c�F@cdZ@b�\@b-@a�#@a��@a�@`��@`�@`�@`�@_�;@^ȴ@^E�@]O�@\�D@[ƨ@["�@Zn�@YG�@X�`@X�u@W�;@W|�@W�@V��@Vff@VV@VE�@V5?@U��@T�j@T9X@S��@S�
@S��@S��@S�@St�@SS�@SC�@S33@S"�@So@So@R�@R�H@R�!@R~�@Rn�@Rn�@R=q@Q�^@Qx�@QX@QG�@Q%@PĜ@P�9@PA�@O�@O�P@Ol�@O\)@O\)@O\)@O;d@N��@N��@Nff@N$�@M��@M�@M`B@M/@L��@LI�@K��@Ko@J�!@JM�@I��@I�#@I��@I�^@I��@Ihs@IG�@I&�@H�`@HQ�@G��@Gl�@F�y@FV@F$�@E@EO�@D�@D�j@D�D@D(�@C�@C33@C"�@B�H@B�\@BJ@A�^@Ahs@AG�@AG�@@�@?K�@?
=@>��@>{@=�h@=O�@=V@<��@<�D@<Z@<�@;�m@;ƨ@;"�@;o@;@:�@:�@:�H@:�@:�@:�@;@;@;@:�H@:�!@:�\@:n�@:M�@:-@9�^@9X@9&�@9%@8��@9%@8�`@8�@8  @7K�@6��@6��@65?@5p�@5�@4��@4j@4(�@41@3�m@3�m@3�m@3�
@3��@2��@2~�@2M�@1��@1%@0��@0��@0��@0��@0Ĝ@0�@/�;@/�@/|�@.�@.��@.ff@.V@.$�@-@-p�@,��@,�/@,�/@,�/@,�/@,�@,j@,j@,I�@,Z@,9X@+�m@+�F@+��@+dZ@+C�@+o@*^5@*-@*�@)�@)��@)��@)�7@)X@)X@)7L@(��@(Ĝ@(�9@(�u@(r�@(r�@(bN@(Q�@(A�@(A�@( �@'�w@'|�@'K�@'+@'
=@&�R@&ff@%�h@%�@$��@$�j@$�D@#��@#�@#t�@#dZ@#S�@#C�@#"�@#o@"�H@"�\@"~�@"^5@"-@"J@!��@!��@!�@!G�@ A�@��@;d@
=@�+@v�@ff@5?@@��@?}@��@��@z�@I�@9X@(�@�m@�F@��@�@33@��@��@n�@=q@-@J@��@��@��@x�@hs@�`@�9@��@�u@�@�@�@r�@r�@Q�@A�@b@�@�@��@ȴ@�R@��@�+@ff@{@@O�@��@�j@�D@9X@�@��@��@dZ@33@@�@�@��@�!@�!@��@��@�\@=q@J@�@�#@��@��@�7@x�@G�@G�@7L@7L@&�@��@��@��@r�@  @  @�@�@��@;d@�@@@�-@�-@�-@�-@�h@�@O�@��@�/@�@9X@��@ƨ@��@�@t�@S�@C�@C�@33@"�@@
��@
��@
�!@
^5@
M�@
�@	��@	�@	X@��@�u@�@bN@  @��@�w@�@�@��@��@�P@�P@|�@\)@�@�@��@��@V@E�@5?@�@V@�@V@��@�@�@�/@��@�D@9X@�@��@�m@�
@��@�@t�@dZ@dZ@S�@33@o@�!@n�@^5@=q@-@J@�@�^@�7@G�@%@ �`@ �`@ Ĝ@   ?�;d?���?��?��?��?���?�V111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�B�XB�}B�}B�}B�wB�wB�wB�wB�wB�}B��B��B��BBŢBȴBȴB��B��B��BBBÖBƨBǮBǮBǮBǮBƨBŢBŢB��B��BB�9B��B�DBt�BR�B7LB'�B�B{B+BBBB	7B1BB��B�yB�mB�ZB�;B��B�dB�3B��B�Bp�BiyB2-B�B\B
=BB
��B
�B
�B
�B
�fB
�ZB
�;B
�B
��B
ŢB
�qB
�9B
�B
��B
��B
��B
��B
��B
��B
�oB
�DB
�B
x�B
r�B
\)B
K�B
G�B
A�B
:^B
1'B
�B
�B
�B
	7B
B	��B	�B	�mB	�ZB	�BB	�#B	ŢB	�LB	�B	��B	��B	�oB	�7B	�bB	�{B	�{B	�PB	�B	�B	o�B	bNB	VB	O�B	J�B	N�B	K�B	A�B	5?B	2-B	/B	-B	)�B	&�B	#�B	�B	�B	uB	
=B	%B	B	  B��B��B��B�B�B�sB�mB�mB�fB�`B�TB�HB�;B�#B�
B�B��B��B��B��BȴBǮBĜB��B�wB�dB�RB�LB�?B�!B�B�B��B��B��B��B��B��B��B��B��B��B�{B�VB�JB�=B�7B�1B�+B�B�B�B�B}�B|�Bz�Bw�Bu�Bt�Bs�Bq�Bo�Bn�Bl�BjBhsBe`BcTBaHB_;B^5B\)BZBVBP�BK�BH�BG�BG�BF�BE�BD�BB�BA�B@�B@�B?}B=qB:^B7LB5?B49B1'B/B-B,B+B'�B$�B �B�B�B�B�B�B�B�B�B�B�B�B�B�BoB�B{BuBuB{BuB{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B{BuB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B"�B"�B#�B#�B#�B$�B$�B%�B&�B)�B.B1'B49B9XB:^B:^B=qB=qB?}BA�BA�BB�BC�BC�BE�BF�BH�BP�BXBXBW
BVBW
BW
BXBXBZB]/BaHBe`BiyBk�Bn�Bq�Bw�B~�B�B�B�B�%B�DB�VB�oB��B��B��B��B��B�B�B�-B�FB�RB�XB�XB�dB�}BɺB��B��B��B��B��B��B�
B�B�#B�#B�5B�BB�TB�ZB�fB�yB�B�B�B�B�B�B��B��B��B	  B	B	B	
=B	VB	VB	\B	bB	bB	oB	�B	�B	�B	�B	�B	�B	 �B	%�B	'�B	)�B	+B	-B	0!B	0!B	1'B	1'B	2-B	33B	49B	49B	5?B	6FB	7LB	8RB	9XB	:^B	<jB	=qB	>wB	@�B	A�B	A�B	A�B	B�B	C�B	D�B	E�B	F�B	F�B	G�B	H�B	H�B	I�B	I�B	L�B	M�B	O�B	O�B	P�B	P�B	P�B	Q�B	R�B	S�B	VB	VB	XB	XB	YB	YB	\)B	]/B	`BB	aHB	bNB	cTB	e`B	gmB	gmB	gmB	hsB	hsB	jB	l�B	l�B	l�B	n�B	p�B	q�B	r�B	s�B	t�B	u�B	v�B	w�B	x�B	y�B	|�B	� B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�%B	�+B	�+B	�1B	�7B	�=B	�=B	�DB	�JB	�\B	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�3B	�?B	�FB	�FB	�RB	�^B	�^B	�^B	�^B	�jB	�}B	��B	ĜB	ƨB	ɺB	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�B	�)B	�;B	�HB	�HB	�NB	�TB	�TB	�TB	�TB	�ZB	�ZB	�ZB	�ZB	�ZB	�ZB	�`B	�`B	�fB	�fB	�mB	�mB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
%B
+B
1B
	7B

=B

=B

=B
DB
JB
PB
PB
PB
VB
\B
bB
hB
hB
hB
oB
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
"�B
$�B
$�B
%�B
%�B
'�B
(�B
(�B
)�B
)�B
)�B
+B
+B
)�B
)�B
)�B
,B
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
1'B
1'B
1'B
33B
33B
33B
33B
49B
49B
5?B
6FB
6FB
6FB
6FB
6FB
7LB
7LB
7LB
8RB
7LB
7LB
8RB
9XB
9XB
9XB
9XB
9XB
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
=qB
>wB
>wB
?}B
?}B
?}B
@�B
@�B
A�B
B�B
B�B
B�B
B�B
C�B
D�B
D�B
D�B
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
F�B
G�B
I�B
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
T�B
T�B
T�B
T�B
T�B
T�B
T�B
VB
VB
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
ZB
ZB
[#B
[#B
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
aHB
bNB
bNB
bNB
bNB
bNB
cTB
cTB
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
jB
jB
jB
k�B
k�B
k�B
l�B
m�B
l�B
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
o�B
o�B
o�B
o�B
p�B
p�B
p�B
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
t�B
t�B
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
y�B
y�B
y�B
z�B
z�B
z�B
z�B
z�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�B�rB�cB�}B��B��B��B�wB�wB��B��B��B��B��B��BżBȴB��B��B��B�VBªBªBÖBƨBǮB��B��BǮB��BżBżB�DB�jB�B��B�xB�.B{0BX�B;�B*�B vBB�B�B�BB
	B
�B	RB��B��B�$B�B�BοB�VB�`B�B�gBs�BnIB4�B�BbB�BB
��B
�B
��B
��B
�RB
�B
�B
ڠB
�6B
ǮB
��B
�B
��B
�fB
�tB
�|B
��B
�!B
��B
��B
�jB
��B
zDB
u%B
^B
M�B
IB
C�B
<6B
3�B
=B
B
�B

�B
�B	��B	�B	�$B	�,B	�B	޸B	�KB	�$B	��B	�B	��B	�,B	�#B	��B	�MB	�B	�B	�mB	��B	q�B	d@B	W�B	Q�B	K�B	O�B	MjB	C{B	6+B	3MB	/�B	-�B	*�B	(
B	$�B	!HB	B	�B	DB	+B	MB	;B��B�B�B�3B�WB��B�B�B��B��B��B��B�vB�B�sBּB�BуBϫB̘B�RBȚBňBªB��B�PB��B�B��B�AB��B�B��B��B��B�tB��B�VB�~B��B��B��B��B��B�6B��B��B��B�1B��B��B�MB�'B~�B}�B|By	Bv`Bu?BtnBr�BpUBo�Bm]Bk�Bi�Bf�Bd@BbB`BB^�B]B[�BYBS�BMjBI�BG�BG�BF�BFtBE�BC�BB'B@�B@�B@�B?B=B8�B5�B6B3B0�B./B,�B,qB*�B(XB#�B�B�BB�B�B�BQBBByBsB�B�BMBmBMB�B�BMBaBMB�B�B�B�B�B�B�B�B�B�B�B�B�B�BBBMBMB?B_B�B�B�B+BEBKBBB�B�B�B�B�BB7B�BBWB~BB�BBpB vB �B# B# B$&B$B$&B%FB%FB&�B'�B+B/5B2B4�B9�B:�B;B=�B=�B?�BA�BBBCBDBDgBF%BG_BJ#BR:BXEBX_BW�BV�BWsBWYBX�BX�BZ�B^Ba�Be�BjBl=Bo5Br|BxRBHB� B�UB�{B��B��B��B��B��B�B�5B�tB�*B�WB�wB��B��B�lB��B��B�B�iB�	B��B�B��B�B�&B�gB�sB�QB�WB�qBޞB��B�B��B��B��B��B�B�B��B��B�hB�LB�DB�(B	 OB	oB	�B	
�B	�B	�B	vB	�B	�B	�B	�B	�B	�B	�B	�B	 'B	!-B	&B	(>B	*KB	+QB	-CB	0!B	0UB	1'B	1AB	2GB	33B	49B	4nB	5ZB	6`B	7�B	8�B	9�B	:�B	<�B	=�B	>�B	@�B	A�B	A�B	A�B	B�B	C�B	D�B	E�B	F�B	F�B	G�B	H�B	H�B	I�B	J#B	MB	M�B	O�B	O�B	QB	QB	QB	R:B	S&B	T,B	VB	VB	X+B	X+B	YKB	YeB	\]B	]dB	`\B	a|B	b�B	c�B	e�B	g�B	g�B	g�B	h�B	h�B	j�B	l�B	l�B	l�B	n�B	p�B	q�B	r�B	s�B	t�B	u�B	v�B	xB	y$B	z*B	}"B	�B	�'B	�B	�'B	�'B	�'B	�B	�'B	�[B	�aB	�?B	�_B	�_B	�fB	�RB	�rB	�rB	�xB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	�B	�@B	�8B	�*B	�0B	�=B	�/B	�5B	�5B	�iB	��B	�hB	�tB	�`B	��B	��B	�xB	�xB	��B	��B	��B	��B	��B	�B	��B	�#B	�B	�VB	�B	�@B	�FB	�9B	�?B	�EB	�1B	�7B	�7B	�QB	�kB	ܒB	�pB	�bB	�bB	�B	�nB	�nB	�B	�nB	�tB	�tB	�tB	�tB	�ZB	�B	�zB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	�B	�B	�B	�B	�B	�B	�(B	�.B	�.B	�.B
 B
 OB
;B
AB
MB
MB
SB
?B
zB
�B
	RB

XB

XB

rB
�B
~B
jB
jB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
# B
$�B
%,B
&2B
&LB
($B
)B
)*B
*B
*B
*B
+B
+B
*0B
*0B
*eB
,"B
-)B
-]B
.cB
/OB
/5B
/B
/B
/OB
/5B
/iB
1AB
1AB
1[B
3hB
3MB
3MB
3hB
4TB
4nB
5tB
6FB
6`B
6FB
6FB
6zB
7fB
7LB
7�B
8lB
7�B
7fB
8lB
9rB
9rB
9rB
9rB
9�B
;�B
;B
;B
;�B
<jB
<�B
<�B
<jB
<�B
<�B
=�B
=�B
=�B
=�B
=qB
=qB
=qB
=�B
=qB
=�B
=�B
>�B
>�B
?�B
?�B
?�B
@�B
@�B
A�B
B�B
B�B
B�B
B�B
C�B
D�B
D�B
D�B
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
F�B
G�B
I�B
KB
K�B
LB
L�B
MB
L�B
L�B
MB
M�B
M�B
N�B
N�B
PB
O�B
O�B
PB
O�B
O�B
Q B
QB
Q B
RB
S&B
S&B
S&B
SB
S&B
SB
R�B
SB
TB
T,B
U2B
UB
T�B
UB
T�B
T�B
T�B
T�B
UB
U2B
UB
VSB
V9B
W$B
W$B
XB
X+B
X+B
X+B
XEB
Y1B
YKB
Z7B
Z7B
[=B
[WB
\CB
\CB
\]B
\CB
]dB
]IB
^5B
^OB
^jB
^OB
^5B
^5B
^OB
^OB
^OB
_VB
_VB
_;B
_;B
_pB
_pB
`BB
`\B
`BB
`BB
`\B
`vB
`\B
`\B
abB
abB
abB
bNB
bhB
bhB
bhB
b�B
c�B
c�B
e�B
e`B
ezB
ezB
ezB
ezB
e�B
f�B
f�B
f�B
f�B
f�B
g�B
g�B
h�B
hsB
h�B
h�B
h�B
hsB
h�B
iyB
i�B
i�B
iyB
i�B
i�B
j�B
j�B
j�B
k�B
k�B
k�B
l�B
m�B
l�B
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
o�B
o�B
o�B
o�B
p�B
p�B
p�B
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
t�B
t�B
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
xB
w�B
w�B
xB
y�B
zB
y�B
z�B
z�B
z�B
z�B
z�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.07(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201812050037202018120500372020181205003720202211182137052022111821370520221118213705201812060021192018120600211920181206002119  JA  ARFMdecpA19c                                                                20181125003621  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20181124153711  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20181124153713  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20181124153713  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20181124153714  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20181124153714  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20181124153714  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20181124153714  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20181124153714  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20181124153714                      G�O�G�O�G�O�                JA  ARUP                                                                        20181124155541                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20181124153641  CV  JULD            G�O�G�O�Fę                JM  ARCAJMQC2.0                                                                 20181204153720  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20181204153720  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20181205152119  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200114171535                      G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221118123705  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221123111508                      G�O�G�O�G�O�                