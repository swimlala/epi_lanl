CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       ~2017-10-20T15:35:44Z creation;2017-10-20T15:35:47Z conversion to V3.1;2019-12-18T07:27:23Z update;2022-11-21T05:31:49Z update;     
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
_FillValue                 �  ]T   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  a@   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �t   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �`   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED            
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
resolution        :�o     �  �$   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �l   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �|   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20171020153544  20221123111507  4902148 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               tA   JA  I1_0397_116                     2C  Dd%�NAVIS_A                         0397                            ARGO 011514                     863 @�/����1   @�/ �d� @;a�7Kƨ�d%�7Kƨ1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BHffBO��BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�<�D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�|�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111��)@�(�@�@�A�HA>�HA^�HA~�HA�p�A�p�A�p�A�p�A�p�A�p�A�p�A�p�B�RB�RB�RB�RB'�RB/�RB7�RB?�RBH�BOQ�BW�RB_�RBg�RBo�RBw�RB�RB��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��
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
��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D��D��D {�D ��D!{�D!��D"{�D"��D#{�D#��D${�D$��D%{�D%��D&{�D&��D'{�D'��D({�D(��D){�D)��D*{�D*��D+{�D+��D,{�D,��D-{�D-��D.{�D.��D/{�D/��D0{�D0��D1{�D1��D2{�D2��D3{�D3��D4{�D4��D5{�D5��D6{�D6��D7{�D7��D8{�D8��D9{�D9��D:{�D:��D;{�D;��D<{�D<��D={�D=��D>{�D>��D?{�D?��D@{�D@��DA{�DA��DB{�DB��DC{�DC��DD{�DD��DE{�DE��DF{�DF��DG{�DG��DH{�DH��DI{�DI��DJ{�DJ��DK{�DK��DL{�DL��DM{�DM��DN{�DN��DO{�DO��DP{�DP��DQ{�DQ��DR{�DR��DS{�DS��DT{�DT��DU{�DU��DV{�DV��DW{�DW��DX{�DX��DY{�DY��DZ{�DZ��D[{�D[��D\{�D\��D]{�D]��D^{�D^��D_{�D_��D`{�D`��Da{�Da��Db{�Db��Dc{�Dc��Dd{�Dd��De{�De��Df{�Df��Dg{�Dg��Dh{�Dh��Di{�Di��Dj{�Dj��Dk{�Dk��Dl{�Dl��Dm{�Dm��Dn{�Dn��Do{�Do��Dp{�Dp��Dq{�Dq��Dr{�Dr��Ds{�Ds��Dt{�Dt��Du{�Du��Dv{�Dv��Dw{�Dw��Dx{�Dx��Dy{�Dy��Dz{�Dz��D{{�D{��D|{�D|��D}{�D}��D~{�D~��D{�D��D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D½�D���D�=�D�}�Dý�D���D�=�D�}�DĽ�D���D�=�D�}�DŽ�D���D�=�D�}�Dƽ�D���D�=�D�}�Dǽ�D���D�=�D�}�DȽ�D���D�=�D�}�Dɽ�D���D�=�D�}�Dʽ�D���D�=�D�}�D˽�D���D�=�D�}�D̽�D���D�=�D�}�Dͽ�D���D�=�D�}�Dν�D���D�=�D�}�DϽ�D���D�=�D�}�Dн�D���D�=�D�}�Dѽ�D���D�=�D�}�Dҽ�D���D�=�D�}�Dӽ�D���D�=�D�}�DԽ�D���D�=�D�}�Dս�D���D�=�D�}�Dֽ�D���D�=�D�}�D׽�D���D�=�D�}�Dؽ�D���D�=�D�}�Dٽ�D���D�=�D�}�Dڽ�D���D�=�D�}�D۽�D���D�:�D�}�Dܽ�D���D�=�D�}�Dݽ�D���D�=�D�}�D޽�D���D�=�D�}�D߽�D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�z�D���D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�@�D�z�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111��A��yA��yA��yA��yA���A���A���A���A���A���A�  A�  A�  A�A�A�A�  A���A�  A�A�A���A�ȴA�|�A�hsA���A���A��
A��RA�JA��A�1A��A��+A��hA�1A�|�A��A��`A��A�t�A��#A��A���A�K�A���A�VA�A���A�I�A�ffA��-A�bA��FA�A�7LA��-A���A�ZA�+A��^A�M�A��^A�=qA�z�A��FA�VA���A�  A�O�A��/A�oA�XA��!A�p�A��9A�ffA���A���A���A�ȴA�VA�AXA}��A}O�A|�yA|E�Az�RAy�
Ayt�Ay33AxQ�Au�mAt��At5?As�-As�7As7LArv�Aq;dApQ�Ap �Ao�Am�7Ak�FAj�Aj$�Ah��Af�AfbAc��Ab��Ab��Ab�9AbI�A`��A`�A_��A_dZA^�9A^jA^A�A^JA]�wA]p�A[�mAY33AX�RAWO�AV�\AU��AS�AS|�ARffAQ��AP��AP1'AO�wAN�jAM�-AM\)AL�`AIAGl�AF�`AD�RACO�ABn�AAdZA@M�A?C�A>^5A="�A<��A;��A;�hA;hsA;hsA;O�A:�/A9|�A8��A7��A7�PA7dZA6�A4�`A2��A1`BA0bNA/ƨA/
=A. �A-x�A-33A,��A,�/A,~�A+�A+�;A+�wA+�A*�+A)`BA(�A(VA'x�A&��A&(�A%�-A%7LA$�HA$v�A#��A"��A"bA!��A!VA�wAbNA�FAn�AbA�/AXA(�A�hA��A��A�AZA�A�AVA/A~�A�A��A��At�A;dA�`A�A��A%A
�HA
�9A
�+A
I�A	�
A�yAx�A�A�`Ar�A�wA&�AjA�w@��
@��@���@���@�;d@���@���@�|�@� �@@���@�o@�R@��@��@�~�@���@��@���@�Ĝ@�I�@�C�@�(�@�?}@��@���@�;d@�ff@�@��@��@��T@ٙ�@؃@�S�@��y@֏\@ղ-@�Q�@�C�@�V@Ѳ-@ѡ�@љ�@љ�@�x�@���@�Z@���@��@́@�x�@�p�@�G�@�&�@��/@�Q�@˅@�`B@���@ȋD@�t�@�S�@�S�@�33@�@Ł@Õ�@��T@��@���@���@�A�@�x�@���@�=q@��@�$�@���@���@���@�E�@��^@�?}@�b@���@��@��@�  @�~�@��@�&�@�bN@�K�@��#@�Q�@�dZ@�+@�~�@��@��@�7L@��@���@��/@��j@�I�@��@�
=@�V@��/@�  @�ƨ@�\)@�;d@��@��@���@�v�@�E�@�$�@�J@��^@�p�@��/@�j@�(�@�"�@�x�@�&�@��@��@��D@�Z@�9X@���@�o@�ȴ@���@��+@�^5@��h@��j@�A�@�(�@��@�K�@�S�@�S�@�\)@�dZ@�+@�@��@��H@��R@���@�n�@�@�@���@�`B@�/@��@���@�Ĝ@��@��u@�z�@�Q�@�1'@��@�b@��;@���@�+@��H@�ff@�{@�/@��u@�(�@��@���@��w@��P@�t�@�\)@�S�@�K�@�C�@�+@�o@��R@�-@��T@�hs@�%@�Ĝ@�A�@��;@���@�dZ@�C�@�33@�"�@��@�@���@��H@���@�v�@��@��@�r�@� �@�  @|�@~��@}�@}�@|I�@z�@z^5@z^5@z^5@z^5@z�@y�^@yG�@y&�@y&�@y&�@y&�@y�@y�@y�@y%@x1'@w��@v��@v�@v��@vff@v$�@uO�@tZ@s��@st�@s33@s@r��@r^5@r=q@q��@q��@q�7@qhs@qG�@p��@p��@pr�@pbN@pbN@pbN@o��@n�@n5?@m�h@m/@mV@lz�@l1@kt�@ko@j��@j�\@jn�@j-@j�@jJ@i��@i�@i��@i��@i��@ihs@iG�@i�@h�u@hA�@h  @g�w@g|�@g+@fȴ@f��@fv�@e��@d�/@d�@c��@cC�@cC�@c"�@c@b�\@bn�@b=q@b-@b-@bJ@a��@a�@a�#@a�^@`bN@^�+@^E�@^$�@^@]�T@]��@]��@]O�@]�@]�@\�@\�j@\��@\j@\(�@[�
@[C�@Z=q@Yx�@Y7L@X��@X1'@W�w@W|�@W�@Vȴ@Vȴ@Vv�@U�@U�@UO�@T�D@S��@SdZ@SS�@SC�@Rn�@Q�^@Q�^@Q�^@Q��@Q�#@R�@RJ@Q��@Q��@Qhs@Q�@P��@PbN@P  @O�@O
=@N��@M��@Mp�@M?}@MO�@MO�@M/@L��@Lj@LI�@L(�@K��@Kƨ@KdZ@K33@J��@JM�@JJ@I�#@IX@I7L@I�@H�`@H�u@H �@G+@FV@E�-@E�h@EO�@E�@D��@D�D@Dz�@Dj@D(�@D�@C��@C��@C�m@C��@Ct�@CdZ@CC�@C"�@B�@B��@B^5@A��@A7L@A%@A%@A%@A�@@��@@r�@@Q�@@1'@@  @?�;@?�w@?�P@?|�@?l�@?;d@>��@>ff@>$�@=p�@<�@<�j@<�D@<j@<I�@<9X@<1@;�F@;ƨ@;�@;33@;o@:�\@:-@9��@9�@9��@97L@9&�@8�`@8Q�@7�P@6ȴ@6�R@65?@5�@5�@4��@4��@4�@4�D@4z�@4j@4j@4Z@49X@4�@3�m@3ƨ@3�F@3�F@3��@3�@3t�@3dZ@333@3"�@2�@2�H@2��@2n�@2�@2J@1��@1��@1�7@1�7@1�7@1�7@1�7@1hs@1X@1�@0Ĝ@0�u@0b@/|�@.�@.ff@-�@,�@,�@,��@,9X@*�@)��@)7L@(Ĝ@(��@(��@(r�@(bN@(A�@(  @'�;@'��@'�w@'�@'�P@'|�@'l�@'K�@&��@&��@&��@&�@&��@&V@%��@%O�@%/@%�@$�@$�D@$j@#S�@"�@"^5@"J@!�@!��@!�^@!�^@!��@!��@!hs@!hs@!X@!G�@!7L@ ��@ �u@ bN@ b@   @�@��@�w@��@V@5?@$�@{@��@�h@?}@/@�/@�@��@9X@�@~�@�@J@��@��@��@��@��@��@�7@G�@&�@&�@��@�9@bN@A�@��@�y@V@@�-@��@��@z�@�
@�@�@�@�@�@�@�@t�@t�@t�@dZ@dZ@dZ@C�@~�@=q@�@hs@X@G�@X@X@G�@G�@X@X@X@X@X@G�@&�@�@�@�@�@�@%@%@%@%@%@��@�`@��@Ĝ@�9@Ĝ@�9@��@�u@�@A�@�@|�@�@��@ff@E�@�@��@V@�@�/@��@�j@�j@�@�@��@�D@j@I�@��@t�@S�@"�@
�@
�@	��@�`@bN@A�@1'@1'@b@�@�@+@�@�R@�R@�+@ff@$�@�T@�@��@�h@p�@`B@O�@/@�@�@V@V@��@j@�@�m@ƨ@��@dZ@33@o@�H@��@��@��@~�@^5@-@J@��@�#@x�@7L@&�@&�@�@ ��@ �`@ ��@ Ĝ@ �9@ �9@ �9@ �9@ �u@ r�@ Q�@ A�@ b?��;?�|�?��?��?���?���?��?��?���?��?��R11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111��A��yA��yA��yA��yA���A���A���A���A���A���A�  A�  A�  A�A�A�A�  A���A�  A�A�A���A�ȴA�|�A�hsA���A���A��
A��RA�JA��A�1A��A��+A��hA�1A�|�A��A��`A��A�t�A��#A��A���A�K�A���A�VA�A���A�I�A�ffA��-A�bA��FA�A�7LA��-A���A�ZA�+A��^A�M�A��^A�=qA�z�A��FA�VA���A�  A�O�A��/A�oA�XA��!A�p�A��9A�ffA���A���A���A�ȴA�VA�AXA}��A}O�A|�yA|E�Az�RAy�
Ayt�Ay33AxQ�Au�mAt��At5?As�-As�7As7LArv�Aq;dApQ�Ap �Ao�Am�7Ak�FAj�Aj$�Ah��Af�AfbAc��Ab��Ab��Ab�9AbI�A`��A`�A_��A_dZA^�9A^jA^A�A^JA]�wA]p�A[�mAY33AX�RAWO�AV�\AU��AS�AS|�ARffAQ��AP��AP1'AO�wAN�jAM�-AM\)AL�`AIAGl�AF�`AD�RACO�ABn�AAdZA@M�A?C�A>^5A="�A<��A;��A;�hA;hsA;hsA;O�A:�/A9|�A8��A7��A7�PA7dZA6�A4�`A2��A1`BA0bNA/ƨA/
=A. �A-x�A-33A,��A,�/A,~�A+�A+�;A+�wA+�A*�+A)`BA(�A(VA'x�A&��A&(�A%�-A%7LA$�HA$v�A#��A"��A"bA!��A!VA�wAbNA�FAn�AbA�/AXA(�A�hA��A��A�AZA�A�AVA/A~�A�A��A��At�A;dA�`A�A��A%A
�HA
�9A
�+A
I�A	�
A�yAx�A�A�`Ar�A�wA&�AjA�w@��
@��@���@���@�;d@���@���@�|�@� �@@���@�o@�R@��@��@�~�@���@��@���@�Ĝ@�I�@�C�@�(�@�?}@��@���@�;d@�ff@�@��@��@��T@ٙ�@؃@�S�@��y@֏\@ղ-@�Q�@�C�@�V@Ѳ-@ѡ�@љ�@љ�@�x�@���@�Z@���@��@́@�x�@�p�@�G�@�&�@��/@�Q�@˅@�`B@���@ȋD@�t�@�S�@�S�@�33@�@Ł@Õ�@��T@��@���@���@�A�@�x�@���@�=q@��@�$�@���@���@���@�E�@��^@�?}@�b@���@��@��@�  @�~�@��@�&�@�bN@�K�@��#@�Q�@�dZ@�+@�~�@��@��@�7L@��@���@��/@��j@�I�@��@�
=@�V@��/@�  @�ƨ@�\)@�;d@��@��@���@�v�@�E�@�$�@�J@��^@�p�@��/@�j@�(�@�"�@�x�@�&�@��@��@��D@�Z@�9X@���@�o@�ȴ@���@��+@�^5@��h@��j@�A�@�(�@��@�K�@�S�@�S�@�\)@�dZ@�+@�@��@��H@��R@���@�n�@�@�@���@�`B@�/@��@���@�Ĝ@��@��u@�z�@�Q�@�1'@��@�b@��;@���@�+@��H@�ff@�{@�/@��u@�(�@��@���@��w@��P@�t�@�\)@�S�@�K�@�C�@�+@�o@��R@�-@��T@�hs@�%@�Ĝ@�A�@��;@���@�dZ@�C�@�33@�"�@��@�@���@��H@���@�v�@��@��@�r�@� �@�  @|�@~��@}�@}�@|I�@z�@z^5@z^5@z^5@z^5@z�@y�^@yG�@y&�@y&�@y&�@y&�@y�@y�@y�@y%@x1'@w��@v��@v�@v��@vff@v$�@uO�@tZ@s��@st�@s33@s@r��@r^5@r=q@q��@q��@q�7@qhs@qG�@p��@p��@pr�@pbN@pbN@pbN@o��@n�@n5?@m�h@m/@mV@lz�@l1@kt�@ko@j��@j�\@jn�@j-@j�@jJ@i��@i�@i��@i��@i��@ihs@iG�@i�@h�u@hA�@h  @g�w@g|�@g+@fȴ@f��@fv�@e��@d�/@d�@c��@cC�@cC�@c"�@c@b�\@bn�@b=q@b-@b-@bJ@a��@a�@a�#@a�^@`bN@^�+@^E�@^$�@^@]�T@]��@]��@]O�@]�@]�@\�@\�j@\��@\j@\(�@[�
@[C�@Z=q@Yx�@Y7L@X��@X1'@W�w@W|�@W�@Vȴ@Vȴ@Vv�@U�@U�@UO�@T�D@S��@SdZ@SS�@SC�@Rn�@Q�^@Q�^@Q�^@Q��@Q�#@R�@RJ@Q��@Q��@Qhs@Q�@P��@PbN@P  @O�@O
=@N��@M��@Mp�@M?}@MO�@MO�@M/@L��@Lj@LI�@L(�@K��@Kƨ@KdZ@K33@J��@JM�@JJ@I�#@IX@I7L@I�@H�`@H�u@H �@G+@FV@E�-@E�h@EO�@E�@D��@D�D@Dz�@Dj@D(�@D�@C��@C��@C�m@C��@Ct�@CdZ@CC�@C"�@B�@B��@B^5@A��@A7L@A%@A%@A%@A�@@��@@r�@@Q�@@1'@@  @?�;@?�w@?�P@?|�@?l�@?;d@>��@>ff@>$�@=p�@<�@<�j@<�D@<j@<I�@<9X@<1@;�F@;ƨ@;�@;33@;o@:�\@:-@9��@9�@9��@97L@9&�@8�`@8Q�@7�P@6ȴ@6�R@65?@5�@5�@4��@4��@4�@4�D@4z�@4j@4j@4Z@49X@4�@3�m@3ƨ@3�F@3�F@3��@3�@3t�@3dZ@333@3"�@2�@2�H@2��@2n�@2�@2J@1��@1��@1�7@1�7@1�7@1�7@1�7@1hs@1X@1�@0Ĝ@0�u@0b@/|�@.�@.ff@-�@,�@,�@,��@,9X@*�@)��@)7L@(Ĝ@(��@(��@(r�@(bN@(A�@(  @'�;@'��@'�w@'�@'�P@'|�@'l�@'K�@&��@&��@&��@&�@&��@&V@%��@%O�@%/@%�@$�@$�D@$j@#S�@"�@"^5@"J@!�@!��@!�^@!�^@!��@!��@!hs@!hs@!X@!G�@!7L@ ��@ �u@ bN@ b@   @�@��@�w@��@V@5?@$�@{@��@�h@?}@/@�/@�@��@9X@�@~�@�@J@��@��@��@��@��@��@�7@G�@&�@&�@��@�9@bN@A�@��@�y@V@@�-@��@��@z�@�
@�@�@�@�@�@�@�@t�@t�@t�@dZ@dZ@dZ@C�@~�@=q@�@hs@X@G�@X@X@G�@G�@X@X@X@X@X@G�@&�@�@�@�@�@�@%@%@%@%@%@��@�`@��@Ĝ@�9@Ĝ@�9@��@�u@�@A�@�@|�@�@��@ff@E�@�@��@V@�@�/@��@�j@�j@�@�@��@�D@j@I�@��@t�@S�@"�@
�@
�@	��@�`@bN@A�@1'@1'@b@�@�@+@�@�R@�R@�+@ff@$�@�T@�@��@�h@p�@`B@O�@/@�@�@V@V@��@j@�@�m@ƨ@��@dZ@33@o@�H@��@��@��@~�@^5@-@J@��@�#@x�@7L@&�@&�@�@ ��@ �`@ ��@ Ĝ@ �9@ �9@ �9@ �9@ �u@ r�@ Q�@ A�@ b?��;?�|�?��?��?���?���?��?��?���?��?��R11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111C�B/B0!B0!B0!B0!B0!B/B0!B/B/B/B/B/B0!B0!B0!B0!B0!B0!B0!B0!B/B-B�B��B��BjBbNBYBL�BH�BE�BC�BB�B?}B>wB=qB9XB6FB2-B,B!�B�B�BhBVBDB%B��B�B��B�LB�'B��B��B�{B�VB�+B� B|�Bw�Bs�Bl�BffB]/BT�BO�BI�B?}B7LB1'B&�B�B
=B
��B
�B
�B
�5B
��B
��B
�?B
�B
��B
��B
�PB
�7B
�B
� B
v�B
p�B
m�B
jB
cTB
S�B
K�B
H�B
E�B
C�B
@�B
;dB
33B
,B
)�B
$�B
�B
DB
%B
  B	��B	�B	�ZB	�B	��B	��B	��B	��B	ĜB	��B	�qB	�XB	�?B	�3B	�-B	�!B	�B	��B	��B	�bB	�JB	�B	|�B	u�B	l�B	hsB	bNB	^5B	YB	S�B	O�B	J�B	D�B	A�B	;dB	)�B	�B	�B	hB		7B	B	  B��B��B�B�B�B�mB�fB�`B�`B�ZB�HB�)B�B�
B��B��B��BɺBÖB�}B�dB�RB�FB�3B�'B�!B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B�{B�hB�bB�JB�+B�B� B|�Bz�By�Bw�Bs�Bs�Br�Bo�Bm�Bk�BhsBe`BcTB_;B^5B]/B]/B]/B]/B\)B[#BZBXBXBW
BVBVBT�BS�BQ�BP�BN�BM�BL�BL�BJ�BI�BG�BF�BE�BD�BC�BB�BA�B@�B=qB9XB6FB5?B5?B49B2-B1'B0!B/B/B0!B/B.B,B)�B)�B)�B,B,B-B.B.B.B-B-B+B)�B)�B)�B(�B(�B(�B)�B+B+B+B+B,B/B0!B.B-B-B-B-B,B,B+B(�B&�B'�B'�B&�B(�B(�B(�B(�B&�B'�B(�B(�B'�B'�B&�B%�B)�B.B.B/B0!B0!B1'B2-B2-B2-B2-B49B49B5?B6FB<jB@�BC�BD�BE�BG�BJ�BO�BR�BR�BVBYBZB\)B\)B]/B]/B]/B_;BaHBcTBe`BjBm�Bn�Bp�Bq�Bq�Br�Bs�Bt�Bu�Bu�Bu�Bw�Bx�B{�B}�B}�B�B�PB�\B�bB�oB�oB�uB�{B��B��B��B��B��B��B��B��B��B��B�B�-B�'B�'B�'B�'B�3B�9B�?B�?B�LB�LB�RB�dB�jB�qB�}B��B��BBĜBŢBŢBƨBǮBȴBɺBɺB��B��B��B��B�
B�B�BB�ZB�fB�mB�mB�yB�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B	B	B	B	B	B	%B	%B	+B	1B		7B	DB	PB	bB	�B	�B	�B	"�B	%�B	&�B	,B	1'B	33B	5?B	7LB	9XB	:^B	;dB	>wB	A�B	C�B	D�B	D�B	D�B	D�B	D�B	D�B	D�B	D�B	H�B	K�B	N�B	N�B	O�B	O�B	P�B	S�B	W
B	ZB	\)B	]/B	^5B	_;B	aHB	aHB	bNB	dZB	dZB	e`B	ffB	gmB	iyB	k�B	k�B	k�B	l�B	q�B	v�B	z�B	|�B	}�B	}�B	�B	�B	�+B	�=B	�JB	�PB	�PB	�VB	�\B	�\B	�\B	�bB	�bB	�hB	�hB	�oB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�'B	�-B	�-B	�-B	�3B	�3B	�3B	�9B	�9B	�XB	��B	B	B	ÖB	ÖB	ÖB	ÖB	ŢB	ƨB	ƨB	ƨB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�#B	�)B	�/B	�5B	�BB	�HB	�HB	�HB	�HB	�TB	�ZB	�`B	�`B	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
+B
1B
	7B
	7B
	7B

=B
DB
DB
DB
DB
JB
JB
JB
JB
PB
PB
PB
PB
VB
VB
VB
\B
bB
hB
hB
hB
hB
hB
hB
oB
uB
uB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
 �B
 �B
 �B
!�B
!�B
!�B
"�B
$�B
%�B
%�B
&�B
'�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
+B
+B
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
0!B
0!B
1'B
1'B
2-B
33B
33B
33B
49B
6FB
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
>wB
?}B
?}B
?}B
?}B
@�B
@�B
B�B
B�B
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
E�B
E�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
I�B
I�B
J�B
J�B
J�B
J�B
K�B
K�B
L�B
L�B
M�B
M�B
M�B
N�B
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
R�B
S�B
S�B
S�B
T�B
VB
W
B
XB
XB
YB
ZB
ZB
[#B
[#B
\)B
[#B
\)B
\)B
\)B
\)B
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
_;B
_;B
_;B
_;B
_;B
_;B
_;B
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
aHB
bNB
bNB
cTB
cTB
cTB
cTB
dZB
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
ffB
gmB
gmB
gmB
gmB
hsB
iyB
jB
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l�B
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
s�B
s�B
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
u�B
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
v�B
v�B
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
x�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111C�B/5B0!B0!B0!B0;B0!B/B0!B/5B/B/B/B/B0!B0!B0!B0!B0;B0;B0UB0�B1B2-B)B	�B��Bn/BfB_�BO\BJ�BG�BEBD�B@�B@B?}B:�B7�B4�B/iB$tBCB�B�B�B6B	B�B��B�HB�XB��B��B�jB��B��B�fB��B~By	BuBm�Bh$B^�BVBQNBK�BA B8�B3MB)DB"NB�B
��B
��B
�B
��B
�:B
�[B
��B
��B
�RB
�B
�"B
��B
�%B
��B
w�B
qAB
ncB
lB
e�B
UgB
LdB
IRB
FB
D3B
A�B
<�B
49B
,�B
+6B
'B
�B
dB
zB
�B	��B	�B	�B	�B	�@B	�NB	��B	�6B	�SB	�AB	�BB	�*B	��B	��B	��B	��B	�B	�"B	�nB	�hB	��B	�gB	~]B	w�B	mwB	i�B	c:B	_VB	ZB	T�B	Q4B	K�B	EmB	C-B	?HB	+�B	!-B	B	&B	
�B	�B	�B�jB�B�B�]B�QB��B�B�B��B�`B��B�/B�B��BյB�FB�oB�0BňB��B�PB�rB�fB�B��B�oB�iB��B��B�qB��B��B�WB�DB��B��B��B��B�]B�QB�EB�+B�YB��B��B��B� B��B�"B��B�GB��B}�B|�B{�By>Bt�Bt�BtBp�Bn�BmBj�BgmBd�B`BB_B]~B]�B]~B]�B\�B[�B[WBX�BXyBWsBVmBV�BU�BU�BS�BR�BPbBN�BM�BM�BK�BK)BI�BGzBF�BE�BD�BC�BB�BA�B@ B<B7�B6�B5�B5%B3�B2B0�B/�B/OB0�B/�B/OB.B+�B*B*�B,�B,�B-wB.IB./B.IB-�B-�B+�B*eB*�B*�B)�B)�B)�B*eB+6B+B+B+kB,�B/�B0�B/OB-wB-)B-CB-CB,=B,qB+�B)�B($B(sB(sB'�B)*B)*B)DB)�B($B)_B)�B)yB(XB(sB'�B'�B+QB/5B/�B0�B1AB0�B1�B2�B2�B2�B2�B4�B4�B6FB7�B=qBA;BC�BESBF�BH�BK�BP}BS@BSuBV�BYeBZ�B\]B\]B]dB]~B]�B_�Ba�BdBffBkBm�Bn�Bp�Bq�Bq�BsBs�BuBu�Bu�BvBxByrB|PB~wB~�B�3B��B��B��B��B��B��B�B��B��B��B��B��B�xB�bB�2B�0B�KB�qB�-B�[B�AB�[B�[B�hB�TB�ZB�tB��B��B��B��B��B��B��B��B��B��BĶBżB��B��B��B��B��B�	B�B�6B�NB�[B׍B��B��B�B�B�B�B�B�B�B�B�B�B��B��B��B�!B��B�B�B�DB�JB�BB	;B	GB	MB	9B	9B	?B	YB	EB	�B		�B	�B	�B	 B	B	�B	 B	# B	&2B	'mB	,qB	1�B	3�B	5tB	7�B	9rB	:xB	;�B	>�B	A�B	C�B	D�B	D�B	D�B	D�B	D�B	D�B	D�B	D�B	H�B	LB	OB	OB	PB	P.B	QNB	TFB	W?B	Z7B	\CB	]IB	^�B	_pB	abB	a|B	b�B	dtB	dtB	e�B	f�B	g�B	i�B	k�B	k�B	k�B	l�B	q�B	wB	{B	}<B	~B	~BB	�AB	�aB	�zB	�rB	�dB	��B	�jB	�pB	�vB	�vB	��B	�}B	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�LB	�DB	�QB	�CB	�/B	�/B	�5B	�OB	�UB	�AB	�GB	�-B	�aB	�MB	�hB	�hB	��B	��B	��B	��B	��B	��B	ðB	ðB	��B	��B	żB	��B	��B	��B	��B	��B	��B	��B	�B	�<B	�:B	�B	�2B	�MB	�?B	�KB	�KB	�7B	�QB	�QB	�WB	�xB	�dB	ޞB	��B	�bB	�|B	�bB	�B	�B	�ZB	�`B	�`B	�mB	�sB	�B	�B	�B	��B	��B	��B	�B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�0B	�B	�B	�"B	�B	�"B	�(B	�.B
 4B
 4B
;B
AB
{B
�B
zB
fB
	RB
	RB
	RB

XB
DB
xB
^B
xB
JB
JB
~B
dB
jB
jB
�B
�B
�B
�B
�B
�B
�B
�B
�B
hB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
 �B
 �B
!B
!�B
!�B
"B
#:B
%B
%�B
&2B
'B
(
B
)*B
)B
*0B
*0B
)�B
)�B
*B
*B
*B
+6B
+B
+B
+B
+B
+B
+6B
,"B
,"B
,"B
,"B
,=B
,=B
,"B
,"B
-)B
-CB
-)B
-)B
.B
.B
./B
.B
.B
./B
./B
./B
./B
/5B
/OB
0oB
0oB
1[B
1vB
2aB
33B
3hB
3�B
4�B
6�B
8�B
9�B
:�B
:xB
:xB
:xB
:�B
:xB
;B
;B
;dB
;�B
<�B
<jB
<�B
<�B
<�B
=qB
=qB
=�B
=�B
=�B
=�B
>�B
?�B
?�B
?�B
?�B
@�B
AB
B�B
B�B
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
E�B
E�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
I�B
I�B
J�B
J�B
J�B
J�B
K�B
K�B
L�B
L�B
M�B
N"B
N<B
O(B
Q B
P�B
P�B
P�B
Q B
RB
Q�B
RB
R B
RB
SB
SB
S&B
S&B
TB
TB
TFB
UMB
V9B
W$B
XEB
X_B
Y1B
Z7B
ZQB
[=B
[=B
\)B
[#B
\CB
\CB
\)B
\)B
\CB
\CB
\)B
\)B
\CB
\]B
\xB
]IB
]dB
]dB
^5B
^5B
^OB
^OB
^OB
^5B
^5B
^5B
^OB
^OB
^OB
^5B
^OB
^5B
_VB
_VB
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_VB
_;B
_;B
_VB
_;B
_VB
_VB
_;B
_;B
_pB
_VB
`vB
`vB
a�B
b�B
b�B
cnB
cnB
c�B
c�B
dtB
e`B
e`B
ezB
e`B
e`B
ezB
ezB
ezB
e�B
ezB
ezB
f�B
g�B
g�B
g�B
g�B
h�B
i�B
j�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l�B
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
s�B
s�B
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
u�B
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
v�B
v�B
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
x�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<5��<���<r{�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.07(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201711020035252017110200352520171102003525202211182132162022111821321620221118213216201804031937472018040319374720180403193747  JA  ARFMdecpA19c                                                                20171021003507  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20171020153544  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20171020153545  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20171020153545  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20171020153546  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20171020153546  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20171020153546  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20171020153546  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20171020153546  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20171020153547                      G�O�G�O�G�O�                JA  ARUP                                                                        20171020155658                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20171020153221  CV  JULD            G�O�G�O�F�x�                JM  ARCAJMQC2.0                                                                 20171101153525  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20171101153525  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180403103747  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200114171529                      G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221118123216  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221123111507                      G�O�G�O�G�O�                