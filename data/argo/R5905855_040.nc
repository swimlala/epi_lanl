CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:17:38Z creation;2022-06-04T19:17:38Z conversion to V3.1      
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
_FillValue                 �  I8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M(   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tx   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �0   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �p   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �(   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ޘ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �    HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �D   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �T   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �X   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �h   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �l   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �p   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20220604191738  20220610151508  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               (A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @�	�^o��1   @�	����H@-�t�j~��c�E����1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0ffB8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�33B�33B���B���Bߙ�B���B�  B�  B�  B�  B�  B�  C   C  C  C  C33C	�fC�fC�fC�fC  C  C  C  C�fC�fC  C   C"�C$33C&  C(  C*  C,  C-�fC/�fC2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp�Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�3D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�y�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @p�@s�
@��R@��A��A<��A\��A|��A�z�A�z�A�z�A�z�A�z�A�z�A�z�A�z�B=qB=qB=qB=qB'=qB/��B7=qB?=qBG=qBO=qBW=qB_=qBg=qBo=qBw=qB=qB���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���BÞ�B���B���B���B���B�k�B�k�B�8RB�k�B瞸B랸BB�B���B���B���C�\C�\C�\C�C	��C��C��C��C�\C�\C�\C�\C��C��C�\C�\C!��C$�C%�\C'�\C)�\C+�\C-��C/��C1�\C3�\C5�\C7�\C9�\C;�\C=�\C?�\CA�\CC�\CE�\CG�\CI�\CK�\CM�\CO�\CQ�\CS�\CU�\CW�\CY�\C[�\C]�\C_�\Ca�\Cc�\Ce�\Cg�\Ci�\Ck�\Cm�\Co��Cq�\Cs�\Cu�\Cw�\Cy�\C{�\C}�\C�\C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��{C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��{C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��D s�D ��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��D	s�D	��D
s�D
��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��D s�D ��D!s�D!��D"s�D"��D#s�D#��D$s�D$��D%s�D%��D&s�D&��D's�D'��D(s�D(��D)s�D)��D*s�D*��D+s�D+��D,s�D,��D-s�D-��D.s�D.��D/s�D/��D0s�D0��D1s�D1��D2s�D2��D3s�D3��D4s�D4��D5s�D5��D6s�D6��D7s�D7��D8s�D8��D9s�D9��D:s�D:��D;s�D;��D<s�D<��D=s�D=��D>s�D>��D?s�D?��D@s�D@��DAs�DA��DBs�DB��DCs�DC��DDs�DD��DEs�DE��DFs�DF��DGs�DG��DHs�DH��DIs�DI��DJs�DJ��DKs�DK��DLs�DL��DMs�DM��DNs�DN��DOs�DO��DPs�DP��DQs�DQ��DRs�DR��DSs�DS��DTs�DT��DUs�DU��DVs�DV��DWs�DW��DXs�DX��DYs�DY��DZs�DZ��D[s�D[��D\s�D\��D]s�D]��D^s�D^��D_s�D_��D`s�D`��Das�Da��Dbs�Db��Dcs�Dc��Dds�Dd��Des�De��Dfs�Df��Dgs�Dg��Dhs�Dh��Dis�Di��Djs�Dj��Dks�Dk��Dls�Dl��Dms�Dm��Dns�Dn��Dos�Do��Dps�Dp��Dqs�Dq��Drs�Dr��Dss�Ds��Dts�Dt��Dus�Du��Dvs�Dv��Dws�Dw��Dxs�Dx��Dys�Dy��Dzs�Dz��D{s�D{��D|s�D|��D}s�D}��D~s�D~��Ds�D��D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D¹�D���D�9�D�y�Dù�D���D�9�D�y�DĹ�D���D�9�D�y�DŹ�D���D�9�D�y�Dƹ�D���D�9�D�y�Dǹ�D���D�9�D�y�Dȹ�D���D�9�D�y�Dɹ�D���D�9�D�y�Dʹ�D���D�9�D�y�D˹�D���D�9�D�y�D̹�D���D�9�D�y�D͹�D���D�9�D�y�Dι�D���D�9�D�y�DϹ�D���D�9�D�y�Dй�D���D�9�D�y�Dѹ�D���D�9�D�y�Dҹ�D���D�9�D�y�Dӹ�D���D�9�D�y�DԹ�D���D�9�D�y�Dչ�D���D�9�D�y�Dֹ�D���D�9�D�y�D׹�D���D�9�D�y�Dع�D��D�9�D�y�Dٹ�D���D�9�D�y�Dڹ�D���D�9�D�y�D۹�D���D�9�D�y�Dܹ�D���D�9�D�y�Dݹ�D���D�9�D�y�D޹�D���D�9�D�y�D߹�D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D���D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D��D�9�D�s�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  AϚ�Aϛ=Aϙ�AϖSAϑhAϒ:AϋxAφ�Aό�Aυ�AυAψ1Aω7Aϊ�Aω�Aω�Aό~AώVAώ�AϒAϕAϖ�Aϗ�AϞ�AϢ�AϥAϧAϩ�Aϰ�Aϱ�Aϲ�Aϳ�AϵA϶AϵtAϱ�Aϱ�AϮ�Aϭ�AϭwAϟ�AϒAω�Aς�A�y�A�s�A�l"A�bA�@�A�/A�&LA��;A���A�@�A�8�A��A���A�H�A��"A�?A� �A� 'A���A�j�A�OBA�T,A�r|A�PA�y>A�ncA��A���A�!�A�H�A�Z�A�.�A��A���A��aA�LdA�JA��VA��?A�A}GEAx�As�HAn��Ag^�Aa�KA_}�A\�eAZn�AY�AW_AV4nAS�APd�AN��AJ�AH��AF�QAED�AC�	A@~(A>'RA<A�A:�oA9�A8�A6tTA5TaA4&�A3o A3�A1��A0�]A/��A.8A-H�A+�A*CA)-�A'��A'�A%Z�A$�PA#��A#�A"jA!�wA �A VArGA�A#�A��AS�AsAK�AuA��Aj�AQ�A��A�aA$tA6zA+A��AA�A]dA�A�jA�)A�A�AP�AX�Av`AxlA'RA*0A��A{JA��A��AQ�A�@A��A  AjA�~A�A��A��A-A�`A�A�AE�A�A
�@A
+�A	��A	I�A��A	�A	=qA	�	A�|A�aA��Av�A�kA��A� A�fAC-A�mA�]A�A+�A��A[�A�
A�3Au�AS&A�Ae�A�A �A ��A ֡A �@��,@�C@�1�@���@�v�@�	�@���@�|@��@��D@���@���@���@��@�+k@�C�@�q@��3@��'@�:�@�GE@���@�"@�{@�G�@�<�@���@�h@�Dg@��@�&�@�m]@�j@��@�K�@���@��@�o@�B[@��>@�P@���@�C-@��@��@��,@�>B@��@ޤ�@ޝI@�`�@ݼ�@��@��}@�F�@��/@�#:@�:�@��@�~(@�ݘ@׃{@�q�@���@��@�y>@��N@�x�@�m]@��@ґ�@�1@��@��&@џV@��@��?@�E�@ά�@�a�@��@�O@�Z@���@�C�@��@��@̋D@� �@˱[@�>�@�ں@ɪ�@�:�@�p�@ɣn@ɫ�@��9@�}�@���@�\�@�&�@���@���@��f@��)@Ƒ @� �@Ĩ�@ä@@��2@�#:@���@���@�y�@�,�@��q@�<6@���@��A@���@���@�u�@�$t@��@��e@��?@��@���@�.I@��@���@�A�@�;�@�=q@�O@���@�8�@�8�@�/�@�h�@�x�@���@��@��@��@�@���@�҉@�F@��V@�j�@�O@��@���@��@��1@�2�@��@�@@���@�]d@�	@��@@�w2@�]�@��@�6�@��Z@���@���@�F�@��e@�\�@�	@���@�P�@��E@��.@��a@�Z�@��@��!@�u�@�S�@�-@�O�@�ں@���@�B[@�4@���@���@�/�@��@��U@���@�Q�@��t@�b�@�'�@��@���@�R�@���@�S&@�@@�%@���@�#:@���@�#�@�_�@��@��@�C@��H@���@��m@�K^@��@���@�P�@�@@��s@���@�M@�
�@��@���@��[@�-w@��h@�$�@�,�@���@��@�xl@���@�7L@��4@���@�oi@�H�@���@���@��k@�Q�@���@�J�@��@�خ@�dZ@��@���@�u%@�O@��
@�t�@�8�@��@��p@�y>@�.�@� �@��@���@��X@�e�@�2a@���@��_@�R�@�e@�خ@���@��@��!@�,=@���@��7@�t�@�e,@�A�@�&@���@��@���@���@��@�L0@��@��@�N<@�%F@��@�($@��m@���@�ݘ@��
@��@�<6@��@��@��b@�r�@�J�@��@���@��@��@��I@�n�@�Ft@�!�@��@��@���@���@�Dg@��j@��@�1�@��;@���@��S@�a�@�#�@���@�W�@�7�@�u@���@�RT@��@�ߤ@��@�_@�u@���@���@�Y�@���@�z�@�Z@�1'@�x@�V@E9@)_@(@~��@~��@~d�@}�3@}B�@|��@|�@{�w@{K�@{(@z��@zȴ@z��@zZ�@y-w@x�$@x�9@x�e@x�@xFt@x~@x	�@w�
@w�0@w�@@w{J@vߤ@v�@v($@u�@s��@s"�@r�@rOv@q��@qԕ@q��@q�@p�@p!@o��@o!-@nL0@m��@m��@m2a@l�5@l��@l��@lg8@lH@lG@k�q@kZ�@kK�@k1�@k�@j�M@j��@j��@j_@i�S@ie,@i(�@h��@h�)@h�o@h%�@g�A@g��@g�{@g�@f�6@f@�@e��@e�@e�=@e�@e[W@e+@d��@d��@d �@c�@c�K@c��@b��@b@�@a�N@a��@`Ĝ@`4n@`_@`r�@`�@_��@_�@^�L@^
�@]��@]�S@]T�@\��@\��@\q@\1@Z�'@ZM�@Z.�@Y�D@Y�@Ya�@Y5�@Xh�@X  @W�K@W�@@Wx@WK�@WC@Vں@V��@V��@VGE@U��@UT�@T�@T��@T]d@S�@Sg�@SF�@R�@R($@Q�C@Qhs@Q:�@P��@P?�@O�[@O!-@N�h@N�@M�@Mo @L�)@L�u@Lh�@LA�@K�F@Jȴ@Jd�@JJ�@J3�@I��@I�@I��@IN<@H�@HA�@G�r@G��@Go�@G�@F��@F�@F;�@E�@E��@E2a@D�?@De�@DM@C��@C�@Cb�@C�@B��@B��@B!�@A�T@AY�@A�@@�	@@��@@A�@?خ@?@>�x@>{�@>C�@>@=�z@=�S@=F@<��@<�?@<��@<M@;��@;O@;4�@;�@:��@9��@9�C@9^�@8��@8�K@8ی@8��@8z�@82�@7iD@6�@6\�@6E�@6�@5�z@5��@5��@5S&@4�@4D�@4�@4�@3��@3��@3�6@3��@2�@2n�@2;�@1��@1�H@1�M@1hs@1?}@0֡@0�Y@0j@/��@/��@/�k@/�4@/~�@/b�@/>�@/4�@/o@.v�@. �@-�>@-^�@-0�@-�@,�f@,��@,bN@,:�@,�@+��@+;d@*��@*��@*n�@*5?@)��@)�3@)J�@(�z@(oi@(S�@(9X@(�@'�;@'��@'"�@&��@&l�@&�@&{@&�@%��@%��@%��@%c@%#�@%	l@$�|@$�Y@$1@#�+@#�
@#�@#g�@#,�@"��@"Ta@"�@!�)@!�@!��@!f�@!N<@!�@ ��@ tT@ Xy@ H@ 7�@ $@�@�[@y�@@O@��@��@��@C�@4@�N@��@7L@�E@��@�O@Xy@~@�@
=@_�@�@��@zx@�@�@�@�a@��@iD@S@��@W�@$�@��@��@��@��@�'@�7@x�@Q�@=�@&�@+@�@�I@j@N�@9X@M@�@�&@خ@�a@�4@F�@$t@��@��@�!@��@Q@�@��@f�@!�@��@�?@�I@�u@]d@	�@�*@�{@a@/�@��@��@��@\�@8�@�@�Z@��@ԕ@�C@&�@�	@��@��@��@�@	�@��@�w@�*@~�@=@�@
��@
��@
p;@
c @
R�@
	@
u@	�z@	��@	o @	IR@	(�@	�@��@��@�@�e@��@N�@~@@�@�m@�K@��@�V@�	1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  AϚ�Aϛ=Aϙ�AϖSAϑhAϒ:AϋxAφ�Aό�Aυ�AυAψ1Aω7Aϊ�Aω�Aω�Aό~AώVAώ�AϒAϕAϖ�Aϗ�AϞ�AϢ�AϥAϧAϩ�Aϰ�Aϱ�Aϲ�Aϳ�AϵA϶AϵtAϱ�Aϱ�AϮ�Aϭ�AϭwAϟ�AϒAω�Aς�A�y�A�s�A�l"A�bA�@�A�/A�&LA��;A���A�@�A�8�A��A���A�H�A��"A�?A� �A� 'A���A�j�A�OBA�T,A�r|A�PA�y>A�ncA��A���A�!�A�H�A�Z�A�.�A��A���A��aA�LdA�JA��VA��?A�A}GEAx�As�HAn��Ag^�Aa�KA_}�A\�eAZn�AY�AW_AV4nAS�APd�AN��AJ�AH��AF�QAED�AC�	A@~(A>'RA<A�A:�oA9�A8�A6tTA5TaA4&�A3o A3�A1��A0�]A/��A.8A-H�A+�A*CA)-�A'��A'�A%Z�A$�PA#��A#�A"jA!�wA �A VArGA�A#�A��AS�AsAK�AuA��Aj�AQ�A��A�aA$tA6zA+A��AA�A]dA�A�jA�)A�A�AP�AX�Av`AxlA'RA*0A��A{JA��A��AQ�A�@A��A  AjA�~A�A��A��A-A�`A�A�AE�A�A
�@A
+�A	��A	I�A��A	�A	=qA	�	A�|A�aA��Av�A�kA��A� A�fAC-A�mA�]A�A+�A��A[�A�
A�3Au�AS&A�Ae�A�A �A ��A ֡A �@��,@�C@�1�@���@�v�@�	�@���@�|@��@��D@���@���@���@��@�+k@�C�@�q@��3@��'@�:�@�GE@���@�"@�{@�G�@�<�@���@�h@�Dg@��@�&�@�m]@�j@��@�K�@���@��@�o@�B[@��>@�P@���@�C-@��@��@��,@�>B@��@ޤ�@ޝI@�`�@ݼ�@��@��}@�F�@��/@�#:@�:�@��@�~(@�ݘ@׃{@�q�@���@��@�y>@��N@�x�@�m]@��@ґ�@�1@��@��&@џV@��@��?@�E�@ά�@�a�@��@�O@�Z@���@�C�@��@��@̋D@� �@˱[@�>�@�ں@ɪ�@�:�@�p�@ɣn@ɫ�@��9@�}�@���@�\�@�&�@���@���@��f@��)@Ƒ @� �@Ĩ�@ä@@��2@�#:@���@���@�y�@�,�@��q@�<6@���@��A@���@���@�u�@�$t@��@��e@��?@��@���@�.I@��@���@�A�@�;�@�=q@�O@���@�8�@�8�@�/�@�h�@�x�@���@��@��@��@�@���@�҉@�F@��V@�j�@�O@��@���@��@��1@�2�@��@�@@���@�]d@�	@��@@�w2@�]�@��@�6�@��Z@���@���@�F�@��e@�\�@�	@���@�P�@��E@��.@��a@�Z�@��@��!@�u�@�S�@�-@�O�@�ں@���@�B[@�4@���@���@�/�@��@��U@���@�Q�@��t@�b�@�'�@��@���@�R�@���@�S&@�@@�%@���@�#:@���@�#�@�_�@��@��@�C@��H@���@��m@�K^@��@���@�P�@�@@��s@���@�M@�
�@��@���@��[@�-w@��h@�$�@�,�@���@��@�xl@���@�7L@��4@���@�oi@�H�@���@���@��k@�Q�@���@�J�@��@�خ@�dZ@��@���@�u%@�O@��
@�t�@�8�@��@��p@�y>@�.�@� �@��@���@��X@�e�@�2a@���@��_@�R�@�e@�خ@���@��@��!@�,=@���@��7@�t�@�e,@�A�@�&@���@��@���@���@��@�L0@��@��@�N<@�%F@��@�($@��m@���@�ݘ@��
@��@�<6@��@��@��b@�r�@�J�@��@���@��@��@��I@�n�@�Ft@�!�@��@��@���@���@�Dg@��j@��@�1�@��;@���@��S@�a�@�#�@���@�W�@�7�@�u@���@�RT@��@�ߤ@��@�_@�u@���@���@�Y�@���@�z�@�Z@�1'@�x@�V@E9@)_@(@~��@~��@~d�@}�3@}B�@|��@|�@{�w@{K�@{(@z��@zȴ@z��@zZ�@y-w@x�$@x�9@x�e@x�@xFt@x~@x	�@w�
@w�0@w�@@w{J@vߤ@v�@v($@u�@s��@s"�@r�@rOv@q��@qԕ@q��@q�@p�@p!@o��@o!-@nL0@m��@m��@m2a@l�5@l��@l��@lg8@lH@lG@k�q@kZ�@kK�@k1�@k�@j�M@j��@j��@j_@i�S@ie,@i(�@h��@h�)@h�o@h%�@g�A@g��@g�{@g�@f�6@f@�@e��@e�@e�=@e�@e[W@e+@d��@d��@d �@c�@c�K@c��@b��@b@�@a�N@a��@`Ĝ@`4n@`_@`r�@`�@_��@_�@^�L@^
�@]��@]�S@]T�@\��@\��@\q@\1@Z�'@ZM�@Z.�@Y�D@Y�@Ya�@Y5�@Xh�@X  @W�K@W�@@Wx@WK�@WC@Vں@V��@V��@VGE@U��@UT�@T�@T��@T]d@S�@Sg�@SF�@R�@R($@Q�C@Qhs@Q:�@P��@P?�@O�[@O!-@N�h@N�@M�@Mo @L�)@L�u@Lh�@LA�@K�F@Jȴ@Jd�@JJ�@J3�@I��@I�@I��@IN<@H�@HA�@G�r@G��@Go�@G�@F��@F�@F;�@E�@E��@E2a@D�?@De�@DM@C��@C�@Cb�@C�@B��@B��@B!�@A�T@AY�@A�@@�	@@��@@A�@?خ@?@>�x@>{�@>C�@>@=�z@=�S@=F@<��@<�?@<��@<M@;��@;O@;4�@;�@:��@9��@9�C@9^�@8��@8�K@8ی@8��@8z�@82�@7iD@6�@6\�@6E�@6�@5�z@5��@5��@5S&@4�@4D�@4�@4�@3��@3��@3�6@3��@2�@2n�@2;�@1��@1�H@1�M@1hs@1?}@0֡@0�Y@0j@/��@/��@/�k@/�4@/~�@/b�@/>�@/4�@/o@.v�@. �@-�>@-^�@-0�@-�@,�f@,��@,bN@,:�@,�@+��@+;d@*��@*��@*n�@*5?@)��@)�3@)J�@(�z@(oi@(S�@(9X@(�@'�;@'��@'"�@&��@&l�@&�@&{@&�@%��@%��@%��@%c@%#�@%	l@$�|@$�Y@$1@#�+@#�
@#�@#g�@#,�@"��@"Ta@"�@!�)@!�@!��@!f�@!N<@!�@ ��@ tT@ Xy@ H@ 7�@ $@�@�[@y�@@O@��@��@��@C�@4@�N@��@7L@�E@��@�O@Xy@~@�@
=@_�@�@��@zx@�@�@�@�a@��@iD@S@��@W�@$�@��@��@��@��@�'@�7@x�@Q�@=�@&�@+@�@�I@j@N�@9X@M@�@�&@خ@�a@�4@F�@$t@��@��@�!@��@Q@�@��@f�@!�@��@�?@�I@�u@]d@	�@�*@�{@a@/�@��@��@��@\�@8�@�@�Z@��@ԕ@�C@&�@�	@��@��@��@�@	�@��@�w@�*@~�@=@�@
��@
��@
p;@
c @
R�@
	@
u@	�z@	��@	o @	IR@	(�@	�@��@��@�@�e@��@N�@~@@�@�m@�K@��@�V@�	1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��B��B�B��B��B��B��B�;B�oB��B��B��B�-B�GB��B��B��B��B��B��B��B��B��B��B��B��B��B��BżB�%B�?B�%B�YB�tB�?B�tB��B��B�EBǮB�+BżBĶB��B�B�[B��B�oB��B�[BߊB	%�B	�eB	�zB
KB
6FB
2B
�2B>�BP�BYKBY�BZBXEBQ4BH�BI7B	B
�VB
�B
��B
ߊB
�wB
�B
��B
��B
o5B
K�B
+B
B	��B	�"B	�|B	�B	��B	�SB	�`B	��B	U�B	3�B	'�B	�B	�B	#:B	'B	$@B	!�B	�B	B�?B�qB�nB�~B��B�JB�uB��B��B��B�_B��B��B��B�IB��B��B��B��B��B�vB�B�B�?B�B��B��B�B�OB~�B|�B|PB{�Bz*BwfBtBr�B~�B~B|PBw�Bs�B|�B�;B�_B�YB��B��B��B��B�B	zB	�B	5B	sB	�B	1B��B	fB	pB	�B	kB	�B	)�B	9	B	@ B	F�B	J�B	R�B	Z�B	_!B	h�B	t�B	{B	��B	��B	� B	poB	e�B	`�B	i*B	o B	q�B	o�B	rB	r|B	r|B	x8B	�UB	�EB	��B	��B	�B	�gB	��B	�B	��B	�CB	�gB	�B	��B	��B	��B	�B	��B	��B	�vB	�TB	�TB	�B	��B	��B	�5B	�]B	�B	��B	�GB	�!B	�iB	��B	�3B	��B	�DB	��B	��B	��B	�B	��B	�]B	�}B	ðB	�B	�JB	�B	�XB	�`B	��B	��B	��B	�B	��B	�wB	��B	��B	�qB	��B	��B	�oB	�B	��B	��B	��B	��B	�%B	�+B	��B	�rB	��B	�B	��B	�XB	�rB	��B	��B	��B	�UB	�3B	��B	�B	�#B	��B	ǮB	�zB	�?B	ĶB	āB	ŢB	�B	ǔB	��B	��B	��B	��B	��B	ɺB	�dB	οB	�VB	�pB	�(B	�hB	�NB	�4B	�[B	�TB	οB	��B	�pB	�hB	ںB	�B	�dB	ݲB	ݘB	�B	޸B	޸B	ޞB	�IB	�7B	ؓB	��B	�B	��B	�|B	�&B	�,B	��B	��B	�B	�B	�0B	�qB	�WB	�=B	�DB	�B	�nB	�bB	�\B	�B	�NB	��B	�B	�B	�5B	�B	��B	�3B	��B	�B	��B	��B	�B	�nB	�3B	�B	�FB	��B	��B	�2B	�B	��B	��B	��B	��B	��B	��B	�B	�MB	��B	�?B	�B	�B	�B	�hB	��B	�PB	�PB	��B	��B	�B	��B	�<B	��B	�VB	��B	��B	�VB	�"B	�B	�B	��B	�VB	�]B	�BB	�B	�BB	�wB	�}B	��B	��B
 B
 �B
;B
-B
�B
�B
B
B
AB
AB
AB
-B
�B
B
�B
�B
�B
B
�B
�B
�B
�B
�B
+B
�B
1B
�B
�B
�B
	RB
	7B
	�B
�B
^B
�B
0B
~B
~B
~B
jB
PB
jB
PB
�B
<B
pB
�B
�B
\B
�B
vB
(B
vB
�B
�B
�B
�B
�B
�B
�B
~B
�B
�B
DB

�B
)B
�B
PB
�B
�B
B
�B
dB
�B
B
B
6B
�B
�B
�B
�B
BB
\B
B
bB
 B
B
NB
�B
�B
�B
B
�B
:B
oB
�B
�B
�B
aB
{B
MB
�B
�B
YB
sB
YB
sB
�B
yB
�B
	B
WB
]B
dB
5B
�B
�B
dB
IB
dB
�B
jB
�B
�B
�B
dB
/B
dB
B
B
~B
jB
!B
�B
!�B
!�B
!�B
"4B
"NB
"NB
"hB
"hB
#B
$@B
$�B
#�B
#�B
$ZB
$@B
$B
$&B
$�B
$�B
%�B
&�B
&�B
&�B
&�B
&�B
(XB
(�B
(�B
)_B
)_B
)�B
)�B
)�B
)DB
*B
*B
*B
*B
*�B
+QB
+�B
-B
-)B
-)B
-)B
-]B
-�B
.�B
.�B
.�B
.}B
.cB
.IB
.cB
.�B
.�B
.�B
/ B
/B
/5B
/�B
0!B
1'B
1vB
1�B
2GB
3B
2�B
2�B
2�B
3�B
49B
4B
33B
3B
2�B
2aB
2-B
1�B
1�B
1�B
1�B
1�B
2-B
2|B
3MB
3�B
4B
4�B
4�B
5B
5%B
5?B
5?B
5�B
5�B
6+B
6B
6`B
6�B
6�B
6�B
6�B
7�B
88B
8lB
8�B
8�B
8�B
8�B
8�B
9	B
8�B
9$B
9>B
9rB
9�B
9�B
:B
:*B
:*B
:^B
:xB
:xB
:�B
;B
;0B
;B
;JB
<PB
<�B
<�B
<�B
=�B
>�B
?�B
@4B
@iB
@�B
@OB
@B
@ B
@ B
@4B
@iB
@�B
@�B
A�B
BB
B�B
B�B
B�B
C-B
C�B
C�B
C�B
EB
EmB
E�B
E�B
F%B
F?B
F%B
FtB
F�B
F�B
F�B
G�B
G�B
H1B
H�B
H�B
H�B
IRB
IB
IlB
I�B
JXB
JrB
JrB
KDB
KDB
K�B
LJB
L~B
MB
L�B
MjB
M�B
NB
N"B
NB
NpB
OBB
O�B
O�B
O�B
O�B
O�B
O�B
O�B
P}B
P�B
P�B
P�B
Q B
Q4B
Q4B
QhB
Q�B
Q�B
R B
RoB
R�B
SB
S[B
S�B
S�B
TB
TaB
T{B
T{B
T�B
T�B
U�B
U�B
U�B
VB
U�B
U�B
V�B
V�B
V�B
V�B
W
B
WsB
WsB
W�B
W�B
W�B
W�B
X_B
X�B
X�B
X�B
X�B
YB
Y�B
Y�B
ZB
Z7B
Z7B
Z7B
Z7B
Z�B
ZkB
[#B
[�B
[�B
[�B
[�B
\CB
\CB
\)B
\]B
\�B
]/B
]IB
]dB
]dB
]dB
]dB
]IB
^B
^OB
^jB
^�B
^�B
_B
^�B
_B
_VB
_�B
_�B
_�B
`'B
`'B
`BB
`'B
`vB
`\B
`\B
`BB
`�B
a-B
aHB
a�B
a�B
a�B
a�B
bB
bhB
b�B
b�B
b�B
c B
cnB
c�B
c�B
c�B
dB
dB
dtB
e,B
e`B
ezB
ezB
e�B
e�B
f2B
f�B
f�B
gmB
g�B
g�B
g�B
g�B
h>B
h�B
h�B
h�B
h�B
hsB
h�B
i_B
i_B
iyB
i�B
i�B
j0B
jB
j�B
j�B
j�B
kB
kB
kB
kB
kQB
l"B
lWB
lqB
l�B
l�B
l�B
m)B
mwB
m�B
m�B
m�B
m�B
m�B
n/B
ncB
n�B
oB
o�B
pB
pB
p;B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q'B
q�B
q�B
rGB
r|B
r�B
r�B
s3B
shB
tB
tTB
tnB
t�B
t�B
t�B
t�B
t�B
t�B
u%B
u%B
u%B
u%B
u%B
u%B
u?B
u?B
u?B
utB
u�B
u�B
u�B
u�B
vB
vFB
vzB
vzB
v�B
v�B
v�B
wLB
wfB
w�B
w�B
x8B
x8B
x�B
x�B
x�B
x�B
y$B
y�B
y�B
y�B
y�B
z*B
z^B
z^B
z�B
z�B
z�B
z�B
{B
{B
{0B
{�B
{�B
{�B
{�B
|6B
|�B
|�B
}"B
}"B
}"B
}<B
}�B
}�B
}�B
~BB
~�B
~�B
~�B
~�B
~�B
HB
}B
�B
�B
�B
�B
�4B
��B
��B
�iB
�iB
��B
��B
�B
�B
�;B
�;B
�UB
�UB
�o1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��B��B�B��B��B��B��B�;B�oB��B��B��B�-B�GB��B��B��B��B��B��B��B��B��B��B��B��B��B��BżB�%B�?B�%B�YB�tB�?B�tB��B��B�EBǮB�+BżBĶB��B�B�[B��B�oB��B�[BߊB	%�B	�eB	�zB
KB
6FB
2B
�2B>�BP�BYKBY�BZBXEBQ4BH�BI7B	B
�VB
�B
��B
ߊB
�wB
�B
��B
��B
o5B
K�B
+B
B	��B	�"B	�|B	�B	��B	�SB	�`B	��B	U�B	3�B	'�B	�B	�B	#:B	'B	$@B	!�B	�B	B�?B�qB�nB�~B��B�JB�uB��B��B��B�_B��B��B��B�IB��B��B��B��B��B�vB�B�B�?B�B��B��B�B�OB~�B|�B|PB{�Bz*BwfBtBr�B~�B~B|PBw�Bs�B|�B�;B�_B�YB��B��B��B��B�B	zB	�B	5B	sB	�B	1B��B	fB	pB	�B	kB	�B	)�B	9	B	@ B	F�B	J�B	R�B	Z�B	_!B	h�B	t�B	{B	��B	��B	� B	poB	e�B	`�B	i*B	o B	q�B	o�B	rB	r|B	r|B	x8B	�UB	�EB	��B	��B	�B	�gB	��B	�B	��B	�CB	�gB	�B	��B	��B	��B	�B	��B	��B	�vB	�TB	�TB	�B	��B	��B	�5B	�]B	�B	��B	�GB	�!B	�iB	��B	�3B	��B	�DB	��B	��B	��B	�B	��B	�]B	�}B	ðB	�B	�JB	�B	�XB	�`B	��B	��B	��B	�B	��B	�wB	��B	��B	�qB	��B	��B	�oB	�B	��B	��B	��B	��B	�%B	�+B	��B	�rB	��B	�B	��B	�XB	�rB	��B	��B	��B	�UB	�3B	��B	�B	�#B	��B	ǮB	�zB	�?B	ĶB	āB	ŢB	�B	ǔB	��B	��B	��B	��B	��B	ɺB	�dB	οB	�VB	�pB	�(B	�hB	�NB	�4B	�[B	�TB	οB	��B	�pB	�hB	ںB	�B	�dB	ݲB	ݘB	�B	޸B	޸B	ޞB	�IB	�7B	ؓB	��B	�B	��B	�|B	�&B	�,B	��B	��B	�B	�B	�0B	�qB	�WB	�=B	�DB	�B	�nB	�bB	�\B	�B	�NB	��B	�B	�B	�5B	�B	��B	�3B	��B	�B	��B	��B	�B	�nB	�3B	�B	�FB	��B	��B	�2B	�B	��B	��B	��B	��B	��B	��B	�B	�MB	��B	�?B	�B	�B	�B	�hB	��B	�PB	�PB	��B	��B	�B	��B	�<B	��B	�VB	��B	��B	�VB	�"B	�B	�B	��B	�VB	�]B	�BB	�B	�BB	�wB	�}B	��B	��B
 B
 �B
;B
-B
�B
�B
B
B
AB
AB
AB
-B
�B
B
�B
�B
�B
B
�B
�B
�B
�B
�B
+B
�B
1B
�B
�B
�B
	RB
	7B
	�B
�B
^B
�B
0B
~B
~B
~B
jB
PB
jB
PB
�B
<B
pB
�B
�B
\B
�B
vB
(B
vB
�B
�B
�B
�B
�B
�B
�B
~B
�B
�B
DB

�B
)B
�B
PB
�B
�B
B
�B
dB
�B
B
B
6B
�B
�B
�B
�B
BB
\B
B
bB
 B
B
NB
�B
�B
�B
B
�B
:B
oB
�B
�B
�B
aB
{B
MB
�B
�B
YB
sB
YB
sB
�B
yB
�B
	B
WB
]B
dB
5B
�B
�B
dB
IB
dB
�B
jB
�B
�B
�B
dB
/B
dB
B
B
~B
jB
!B
�B
!�B
!�B
!�B
"4B
"NB
"NB
"hB
"hB
#B
$@B
$�B
#�B
#�B
$ZB
$@B
$B
$&B
$�B
$�B
%�B
&�B
&�B
&�B
&�B
&�B
(XB
(�B
(�B
)_B
)_B
)�B
)�B
)�B
)DB
*B
*B
*B
*B
*�B
+QB
+�B
-B
-)B
-)B
-)B
-]B
-�B
.�B
.�B
.�B
.}B
.cB
.IB
.cB
.�B
.�B
.�B
/ B
/B
/5B
/�B
0!B
1'B
1vB
1�B
2GB
3B
2�B
2�B
2�B
3�B
49B
4B
33B
3B
2�B
2aB
2-B
1�B
1�B
1�B
1�B
1�B
2-B
2|B
3MB
3�B
4B
4�B
4�B
5B
5%B
5?B
5?B
5�B
5�B
6+B
6B
6`B
6�B
6�B
6�B
6�B
7�B
88B
8lB
8�B
8�B
8�B
8�B
8�B
9	B
8�B
9$B
9>B
9rB
9�B
9�B
:B
:*B
:*B
:^B
:xB
:xB
:�B
;B
;0B
;B
;JB
<PB
<�B
<�B
<�B
=�B
>�B
?�B
@4B
@iB
@�B
@OB
@B
@ B
@ B
@4B
@iB
@�B
@�B
A�B
BB
B�B
B�B
B�B
C-B
C�B
C�B
C�B
EB
EmB
E�B
E�B
F%B
F?B
F%B
FtB
F�B
F�B
F�B
G�B
G�B
H1B
H�B
H�B
H�B
IRB
IB
IlB
I�B
JXB
JrB
JrB
KDB
KDB
K�B
LJB
L~B
MB
L�B
MjB
M�B
NB
N"B
NB
NpB
OBB
O�B
O�B
O�B
O�B
O�B
O�B
O�B
P}B
P�B
P�B
P�B
Q B
Q4B
Q4B
QhB
Q�B
Q�B
R B
RoB
R�B
SB
S[B
S�B
S�B
TB
TaB
T{B
T{B
T�B
T�B
U�B
U�B
U�B
VB
U�B
U�B
V�B
V�B
V�B
V�B
W
B
WsB
WsB
W�B
W�B
W�B
W�B
X_B
X�B
X�B
X�B
X�B
YB
Y�B
Y�B
ZB
Z7B
Z7B
Z7B
Z7B
Z�B
ZkB
[#B
[�B
[�B
[�B
[�B
\CB
\CB
\)B
\]B
\�B
]/B
]IB
]dB
]dB
]dB
]dB
]IB
^B
^OB
^jB
^�B
^�B
_B
^�B
_B
_VB
_�B
_�B
_�B
`'B
`'B
`BB
`'B
`vB
`\B
`\B
`BB
`�B
a-B
aHB
a�B
a�B
a�B
a�B
bB
bhB
b�B
b�B
b�B
c B
cnB
c�B
c�B
c�B
dB
dB
dtB
e,B
e`B
ezB
ezB
e�B
e�B
f2B
f�B
f�B
gmB
g�B
g�B
g�B
g�B
h>B
h�B
h�B
h�B
h�B
hsB
h�B
i_B
i_B
iyB
i�B
i�B
j0B
jB
j�B
j�B
j�B
kB
kB
kB
kB
kQB
l"B
lWB
lqB
l�B
l�B
l�B
m)B
mwB
m�B
m�B
m�B
m�B
m�B
n/B
ncB
n�B
oB
o�B
pB
pB
p;B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q'B
q�B
q�B
rGB
r|B
r�B
r�B
s3B
shB
tB
tTB
tnB
t�B
t�B
t�B
t�B
t�B
t�B
u%B
u%B
u%B
u%B
u%B
u%B
u?B
u?B
u?B
utB
u�B
u�B
u�B
u�B
vB
vFB
vzB
vzB
v�B
v�B
v�B
wLB
wfB
w�B
w�B
x8B
x8B
x�B
x�B
x�B
x�B
y$B
y�B
y�B
y�B
y�B
z*B
z^B
z^B
z�B
z�B
z�B
z�B
{B
{B
{0B
{�B
{�B
{�B
{�B
|6B
|�B
|�B
}"B
}"B
}"B
}<B
}�B
}�B
}�B
~BB
~�B
~�B
~�B
~�B
~�B
HB
}B
�B
�B
�B
�B
�4B
��B
��B
�iB
�iB
��B
��B
�B
�B
�;B
�;B
�UB
�UB
�o1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105235  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604191738  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604191738  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604191738                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605041747  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605041747  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610151508                      G�O�G�O�G�O�                