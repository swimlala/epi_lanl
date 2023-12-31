CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:50:35Z creation;2022-06-04T17:50:36Z conversion to V3.1      
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
_FillValue                 �  I@   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M0   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �P   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �@   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL_ADJUSTED            
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
resolution        :�o     �  �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �<   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �@   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �D   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �H   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �L   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604175035  20220610141505  5905854                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               "A   JA                                  2B  A   APEX                            8422                            2.11.2                          846 @�����1   @��� �.E@0��S���c!\(�1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @33@y��@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B���B�33B�  B�  B�  B�  B���B�  B���B�  B�  B�  B�33B�33B�ffB���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�ffB���B�  B�  C   C  C  C  C  C
  C  C  C�fC�fC�fC�fC�fC�fC  C  C   C"  C$  C&  C(  C*  C,  C.  C0�C2�C3�fC5�fC7�fC:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV�CX�CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  Dy�D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D{��D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D�|�Dؼ�D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D���D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�C3D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@
=@mp�@��@��A��A<��A\��A|��A�z�A�z�A�z�A�z�A�z�A�z�A�z�A�z�B=qB=qB=qB=qB'=qB/=qB7=qB?=qBG=qBO=qBW=qB_=qBg=qBo=qBw=qB=qB���B�8RB���B���B���B���B���B�8RB���B�k�B���B���B���B���B���B�B�k�BǞ�B˞�BϞ�BӞ�Bמ�B۞�Bߞ�B㞸B瞸B���B�B�k�B���B���B���C�\C�\C�\C�\C	�\C�\C�\C��C��C��C��C��C��C�\C�\C�\C!�\C#�\C%�\C'�\C)�\C+�\C-�\C/��C1��C3��C5��C7��C9�\C;�\C=�\C?�\CA�\CC�\CE�\CG�\CI�\CK�\CM�\CO�\CQ�\CS�\CU��CW��CY�\C[�\C]�\C_�\Ca�\Cc�\Ce�\Cg�\Ci�\Ck�\Cm�\Co�\Cq�\Cs�\Cu�\Cw�\Cy�\C{�\C}�\C�\C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��{C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D s�D ��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��D	s�D	��D
s�D
��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��DmqD�qDs�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��D s�D ��D!s�D!��D"s�D"��D#s�D#��D$s�D$��D%s�D%��D&s�D&��D's�D'��D(s�D(��D)s�D)��D*s�D*��D+s�D+��D,s�D,��D-s�D-��D.s�D.��D/s�D/��D0s�D0��D1s�D1��D2s�D2��D3s�D3��D4s�D4��D5s�D5��D6s�D6��D7s�D7��D8s�D8��D9s�D9��D:s�D:��D;s�D;��D<s�D<��D=s�D=��D>s�D>��D?s�D?��D@s�D@��DAs�DA��DBs�DB��DCs�DC��DDs�DD��DEs�DE��DFs�DF��DGs�DG��DHs�DH��DIs�DI��DJs�DJ��DKs�DK��DLs�DL��DMs�DM��DNs�DN��DOs�DO��DPs�DP��DQs�DQ��DRs�DR��DSs�DS��DTs�DT��DUs�DU��DVs�DV��DWs�DW��DXs�DX��DYs�DY��DZs�DZ��D[s�D[��D\s�D\��D]s�D]��D^s�D^��D_s�D_��D`s�D`��Das�Da��Dbs�Db��Dcs�Dc��Dds�Dd��Des�De��Dfs�Df��Dgs�Dg��Dhs�Dh��Dis�Di��Djs�Dj��Dks�Dk��Dls�Dl��Dms�Dm��Dns�Dn��Dos�Do��Dps�Dp��Dqs�Dq��Drs�Dr��Dss�Ds��Dts�Dt��Dus�Du��Dvs�Dv��Dws�Dw��Dxs�Dx��Dys�Dy��Dzs�Dz��D{s�D{�qD|s�D|��D}s�D}��D~s�D~��Ds�D��D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D¹�D���D�9�D�y�Dù�D���D�9�D�y�DĹ�D���D�9�D�y�DŹ�D���D�9�D�y�Dƹ�D���D�9�D�y�Dǹ�D���D�9�D�y�Dȹ�D���D�9�D�y�Dɹ�D���D�9�D�y�Dʹ�D���D�9�D�y�D˹�D���D�9�D�y�D̹�D���D�9�D�y�D͹�D���D�9�D�y�Dι�D���D�9�D�y�DϹ�D���D�9�D�y�Dй�D���D�9�D�y�Dѹ�D���D�9�D�y�Dҹ�D���D�9�D�y�Dӹ�D���D�9�D�y�DԹ�D���D�9�D�y�Dչ�D���D�9�D�y�Dֹ�D���D�9�D�y�D׹�D���D�9�D�v�Dض�D���D�9�D�y�Dٹ�D���D�9�D�y�Dڹ�D���D�9�D�y�D۹�D���D�9�D�y�Dܹ�D���D�9�D�y�Dݹ�D���D�9�D�y�D޹�D���D�9�D�y�D߹�D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D���D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�=D�y�D��D���D�9�D�y�D��D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A��;A��AA���A���A��ZA��ZA���A���A��8A��>A���A���A���A���A���A���A��]A���A�  A� iA�A�AA�oA�uA�A��A��A�A��lA��MA��|A�|A��A�J�A�IA���AŦ�A�'�A�A�m�A��KA��+A���A��A���A��A���A��0A��UA���A�RTA��XA��aA�aA��NA�0UA���A�g�A���A��A�A�רA��KA�[#A��rA���A���A�ɺA��A��A�eA�p�A�#A��HA�n�A���A���A��A��kA�@A�cTA��A�>�A�S[A�A��wA�� A)�A|�Az>BAr�Aj��Ac֡A`H�A]�CA[��AY$�ATt�AP�AM��AK��AGQAF=�AB�A>��A=@OA<�{A;a|A:��A8�A8�AA7�A3�A/��A.MjA,��A*jA(��A)-A&��A&��A#MjA!��A�/AHA��A  �A!uA!ZA!�A!��A!dZA!&�A! iA g8A��A��A�AA�A`BA�DA�7A�AMA�Ah�A��A��A��A2�A��A�AԕA��A�A�A�zA�A�A�5A�PA�A<�A^5A��Ao�AD�AA�^A��A�_A�.At�A(A�AN<A�mA�+A��AAqA�A
��A
_A
&�A
�A
_A	�>A	��A	��A	��A	l�A	O�A�6AhsA��AoiACA�A�AYKA�QA��A�A!�A�fA�A�mA� A?Ac�A �hA dZA 5�@��^@��S@�C�@�I�@���@�Dg@�s@��f@�C-@��[@��@���@��@��1@�c @��@�y�@���@���@���@�Z@��"@��@�%�@�p�@�~(@�C-@��@��@�G@�o @�@�`�@��K@��@���@�K�@���@�6�@릵@�[W@�&@���@��@涮@�[W@䅈@�<6@�@���@��&@�_p@�^5@߽�@�Mj@��"@���@�`�@�_p@�5?@�"�@�҉@�B[@���@��c@ج@ؐ.@��W@�s@�C�@�+@��@���@�c @��@վw@��@��j@�F@�	l@Ҋr@��@т�@�F@�Y@���@ЂA@Ь�@�*�@�2a@ΔF@͉7@�e�@�5�@��@��)@�tT@�=q@ˮ@�C�@�Ɇ@�8�@��@�b�@�S�@�J#@��@�#:@���@ǩ*@�J�@�%@ƺ�@�H@��@Īe@�N�@��&@Ó�@� i@°�@�c�@��@���@�k�@�@��o@�#:@��@�J�@��'@�r�@��@��n@���@��@��@�R�@���@�4@���@���@��&@�f�@�-w@�|�@��j@���@�y�@�0�@��@��E@�?@���@�(@��p@��h@���@�L0@�'R@��T@���@�c�@��@�:*@���@�\)@���@��D@�H�@�7@���@� i@�m�@�Ov@���@�Mj@�.I@�@@���@�_@�+k@�M@��@��[@�c�@�@���@�]d@�C�@���@���@�c@�)_@��H@��@��<@��@�.�@���@�hs@��]@�Z@��@��@�f�@��E@�Ft@�u@���@�q@�  @�l�@��s@�5?@��=@�_p@�@��9@��D@��@�=�@��e@�_�@�*�@���@�c@�_p@�0�@���@�� @�;�@��+@���@��V@��{@�p�@�o�@�33@�%@��X@�z@�$@���@��w@���@�?}@��e@�h�@��@��@���@���@��	@�]�@�<6@��@���@���@�p;@��@��*@�J�@�2a@�(�@�!-@�
=@��c@��@�z�@�8�@��&@���@�P�@�33@�!-@��@�Ɇ@�h�@�"h@��T@�_p@�B�@��@�ߤ@�Ĝ@���@�u�@�@���@�j@�@��e@�Ov@�	@���@�@��^@���@�W?@�S�@�Mj@�9�@�S@��5@��@���@�Z�@�!@���@���@�l�@�>�@��@���@���@�u%@�Xy@���@��C@�n/@�+�@���@��_@�V@��g@��@�Y�@�'�@� i@�҉@��@�PH@�@���@���@�s@�$t@���@��Y@� �@�{J@�?}@� \@���@���@��\@�8�@��@Z�@~�m@~W�@}�t@}x�@}(�@|��@|@{_p@z�1@zl�@z4@yϫ@y�7@y	l@x�@x�z@x:�@w��@wC�@v��@u�D@u�S@uf�@u\�@u?}@t��@tm�@s�+@s�0@sy�@r��@rs�@q�@qrG@q8�@p�@pz�@o�}@o.I@n��@nn�@n_@m�"@m!�@l�@lc�@l�@k�}@k��@k/�@k)_@ko@j�m@j��@ju%@jkQ@j?@jO@i�>@i�S@iL�@hѷ@h�@g��@g)_@g�@f�@f�@f
�@e�n@e?}@d�$@dM@d@c�0@c!-@b�]@b�!@b�x@b}V@bH�@a-w@`Xy@`M@_�@_�	@_Mj@_�@^M�@]�C@]�"@]Vm@]�@\q@[��@[x@[J#@[�@Z�x@Z3�@Y�D@Y�@YY�@Y�@X�@Xj@X1@W��@W,�@V��@V?@Uԕ@U�@U�^@U�=@U\�@U:�@T�K@T�.@T6@T�@S�w@S�q@So�@R�"@R��@R��@RL0@R#:@Q�D@Q�9@Q��@Q0�@PĜ@O�g@O/�@N��@N.�@M�=@M0�@M@@L��@L�|@L�@Lc�@L%�@K�r@K�w@K�{@KC�@J͟@Jl�@I��@I|@H�K@H�E@H��@HbN@H*�@G�+@Gƨ@G��@G��@GX�@G�@Fߤ@F��@FV@E�D@Eu�@E:�@D�K@D��@D��@D|�@DH@D$@D@C�W@C��@C9�@B��@BQ@B;�@A�D@A��@A�"@Ak�@@�I@@(�@?�@?��@?Z�@>�B@>=q@=��@=��@=Dg@=%@<I�@<�@;�P@;�@:�y@:��@:u%@:�@9�@9O�@9	l@8%�@6��@6��@5��@5u�@5L�@5�@4�|@4D�@3�@3�6@3��@3!-@2ں@2��@2p;@2L0@2#:@2�@1��@1s�@1Q�@1�@0�@0��@0��@0z�@0tT@0e�@0(�@/��@/�@/��@/g�@/W?@/J#@/)_@/�@/�@.��@.�@.�'@.��@.�r@.Q@-�t@-o @-J�@--w@,�/@,�@,H@,9X@+�]@+�6@+��@+�k@+n/@+4�@*�M@*�R@*q�@*�@)�@)�@)ԕ@)��@)m]@)J�@)?}@)-w@(�5@(�@(��@(�D@(U2@(6@(�@'�@'��@'�k@'=@&�c@&s�@&�@%�@%w2@%&�@$��@$ѷ@$�@$j@$U2@$<�@#�m@#��@#�f@#X�@#4�@#S@"�]@"�@"0U@!��@!�C@!�h@!|@!hs@!J�@!�@ �I@ 6@�F@J#@!-@
=@��@�R@0U@�@�)@�9@�^@�M@N<@�@��@��@c�@]d@>B@�@��@_p@,�@�]@��@�R@�L@\�@=q@+k@��@�X@j@(�@��@�)@��@��@A�@,=@M@�@��@�@�:@|�@W?@
=@�]@�x@� @q�@O@�@�t@�7@G�@4@	l@�@_@7@��@�q@��@��@��@x@�@҉@�@�C@�S@��@o @Vm@?}@�@��@|�@bN@I�@"h@G@�Q@�*@�k@e�@�@��@�@a|@&�@��@�n@u�@!�@%@�@��@��@Ft@�@�[@b�@E9@.I@!-@
�M@
�R@
��@
�@
xl@
c @
E�@
-@
�@	�@	��@	zx@	c�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A��;A��AA���A���A��ZA��ZA���A���A��8A��>A���A���A���A���A���A���A��]A���A�  A� iA�A�AA�oA�uA�A��A��A�A��lA��MA��|A�|A��A�J�A�IA���AŦ�A�'�A�A�m�A��KA��+A���A��A���A��A���A��0A��UA���A�RTA��XA��aA�aA��NA�0UA���A�g�A���A��A�A�רA��KA�[#A��rA���A���A�ɺA��A��A�eA�p�A�#A��HA�n�A���A���A��A��kA�@A�cTA��A�>�A�S[A�A��wA�� A)�A|�Az>BAr�Aj��Ac֡A`H�A]�CA[��AY$�ATt�AP�AM��AK��AGQAF=�AB�A>��A=@OA<�{A;a|A:��A8�A8�AA7�A3�A/��A.MjA,��A*jA(��A)-A&��A&��A#MjA!��A�/AHA��A  �A!uA!ZA!�A!��A!dZA!&�A! iA g8A��A��A�AA�A`BA�DA�7A�AMA�Ah�A��A��A��A2�A��A�AԕA��A�A�A�zA�A�A�5A�PA�A<�A^5A��Ao�AD�AA�^A��A�_A�.At�A(A�AN<A�mA�+A��AAqA�A
��A
_A
&�A
�A
_A	�>A	��A	��A	��A	l�A	O�A�6AhsA��AoiACA�A�AYKA�QA��A�A!�A�fA�A�mA� A?Ac�A �hA dZA 5�@��^@��S@�C�@�I�@���@�Dg@�s@��f@�C-@��[@��@���@��@��1@�c @��@�y�@���@���@���@�Z@��"@��@�%�@�p�@�~(@�C-@��@��@�G@�o @�@�`�@��K@��@���@�K�@���@�6�@릵@�[W@�&@���@��@涮@�[W@䅈@�<6@�@���@��&@�_p@�^5@߽�@�Mj@��"@���@�`�@�_p@�5?@�"�@�҉@�B[@���@��c@ج@ؐ.@��W@�s@�C�@�+@��@���@�c @��@վw@��@��j@�F@�	l@Ҋr@��@т�@�F@�Y@���@ЂA@Ь�@�*�@�2a@ΔF@͉7@�e�@�5�@��@��)@�tT@�=q@ˮ@�C�@�Ɇ@�8�@��@�b�@�S�@�J#@��@�#:@���@ǩ*@�J�@�%@ƺ�@�H@��@Īe@�N�@��&@Ó�@� i@°�@�c�@��@���@�k�@�@��o@�#:@��@�J�@��'@�r�@��@��n@���@��@��@�R�@���@�4@���@���@��&@�f�@�-w@�|�@��j@���@�y�@�0�@��@��E@�?@���@�(@��p@��h@���@�L0@�'R@��T@���@�c�@��@�:*@���@�\)@���@��D@�H�@�7@���@� i@�m�@�Ov@���@�Mj@�.I@�@@���@�_@�+k@�M@��@��[@�c�@�@���@�]d@�C�@���@���@�c@�)_@��H@��@��<@��@�.�@���@�hs@��]@�Z@��@��@�f�@��E@�Ft@�u@���@�q@�  @�l�@��s@�5?@��=@�_p@�@��9@��D@��@�=�@��e@�_�@�*�@���@�c@�_p@�0�@���@�� @�;�@��+@���@��V@��{@�p�@�o�@�33@�%@��X@�z@�$@���@��w@���@�?}@��e@�h�@��@��@���@���@��	@�]�@�<6@��@���@���@�p;@��@��*@�J�@�2a@�(�@�!-@�
=@��c@��@�z�@�8�@��&@���@�P�@�33@�!-@��@�Ɇ@�h�@�"h@��T@�_p@�B�@��@�ߤ@�Ĝ@���@�u�@�@���@�j@�@��e@�Ov@�	@���@�@��^@���@�W?@�S�@�Mj@�9�@�S@��5@��@���@�Z�@�!@���@���@�l�@�>�@��@���@���@�u%@�Xy@���@��C@�n/@�+�@���@��_@�V@��g@��@�Y�@�'�@� i@�҉@��@�PH@�@���@���@�s@�$t@���@��Y@� �@�{J@�?}@� \@���@���@��\@�8�@��@Z�@~�m@~W�@}�t@}x�@}(�@|��@|@{_p@z�1@zl�@z4@yϫ@y�7@y	l@x�@x�z@x:�@w��@wC�@v��@u�D@u�S@uf�@u\�@u?}@t��@tm�@s�+@s�0@sy�@r��@rs�@q�@qrG@q8�@p�@pz�@o�}@o.I@n��@nn�@n_@m�"@m!�@l�@lc�@l�@k�}@k��@k/�@k)_@ko@j�m@j��@ju%@jkQ@j?@jO@i�>@i�S@iL�@hѷ@h�@g��@g)_@g�@f�@f�@f
�@e�n@e?}@d�$@dM@d@c�0@c!-@b�]@b�!@b�x@b}V@bH�@a-w@`Xy@`M@_�@_�	@_Mj@_�@^M�@]�C@]�"@]Vm@]�@\q@[��@[x@[J#@[�@Z�x@Z3�@Y�D@Y�@YY�@Y�@X�@Xj@X1@W��@W,�@V��@V?@Uԕ@U�@U�^@U�=@U\�@U:�@T�K@T�.@T6@T�@S�w@S�q@So�@R�"@R��@R��@RL0@R#:@Q�D@Q�9@Q��@Q0�@PĜ@O�g@O/�@N��@N.�@M�=@M0�@M@@L��@L�|@L�@Lc�@L%�@K�r@K�w@K�{@KC�@J͟@Jl�@I��@I|@H�K@H�E@H��@HbN@H*�@G�+@Gƨ@G��@G��@GX�@G�@Fߤ@F��@FV@E�D@Eu�@E:�@D�K@D��@D��@D|�@DH@D$@D@C�W@C��@C9�@B��@BQ@B;�@A�D@A��@A�"@Ak�@@�I@@(�@?�@?��@?Z�@>�B@>=q@=��@=��@=Dg@=%@<I�@<�@;�P@;�@:�y@:��@:u%@:�@9�@9O�@9	l@8%�@6��@6��@5��@5u�@5L�@5�@4�|@4D�@3�@3�6@3��@3!-@2ں@2��@2p;@2L0@2#:@2�@1��@1s�@1Q�@1�@0�@0��@0��@0z�@0tT@0e�@0(�@/��@/�@/��@/g�@/W?@/J#@/)_@/�@/�@.��@.�@.�'@.��@.�r@.Q@-�t@-o @-J�@--w@,�/@,�@,H@,9X@+�]@+�6@+��@+�k@+n/@+4�@*�M@*�R@*q�@*�@)�@)�@)ԕ@)��@)m]@)J�@)?}@)-w@(�5@(�@(��@(�D@(U2@(6@(�@'�@'��@'�k@'=@&�c@&s�@&�@%�@%w2@%&�@$��@$ѷ@$�@$j@$U2@$<�@#�m@#��@#�f@#X�@#4�@#S@"�]@"�@"0U@!��@!�C@!�h@!|@!hs@!J�@!�@ �I@ 6@�F@J#@!-@
=@��@�R@0U@�@�)@�9@�^@�M@N<@�@��@��@c�@]d@>B@�@��@_p@,�@�]@��@�R@�L@\�@=q@+k@��@�X@j@(�@��@�)@��@��@A�@,=@M@�@��@�@�:@|�@W?@
=@�]@�x@� @q�@O@�@�t@�7@G�@4@	l@�@_@7@��@�q@��@��@��@x@�@҉@�@�C@�S@��@o @Vm@?}@�@��@|�@bN@I�@"h@G@�Q@�*@�k@e�@�@��@�@a|@&�@��@�n@u�@!�@%@�@��@��@Ft@�@�[@b�@E9@.I@!-@
�M@
�R@
��@
�@
xl@
c @
E�@
-@
�@	�@	��@	zx@	c�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�rB�XB�=B�XB�=B�rB�XB�rBʌB��B��B��B��BʌB�XB�=B�XB�=B�XB�XB�XB�rBʌB��B��B�B��BʌB�~B�BB�B��B	5%B	qAB	xlB
d&B
\�B
raB
��B
�SB
�?B
��B
�~B
��B
�B
�-B!�BjB
�3BIlB��B�^BՁB��B��B�B�B�B<B�xB�B�$B�XB��B�"B�cB��BՁB�EBںB�aB��Bn�BSBP�B>wB4TB-�B&�B�B	B
��B
�OB
��B
�B
t�B
G_B
(sB	��B	�B	��B	��B	`\B	E�B	6+B	!|B	�B	
�B�;B�=B�RB��B�dB	-B	�B	B	0B	�B	,�B	T�B	P}B	YKB	hsB	RTB	9rB	/ B	# B	B	�B	pB	�B	�B	�B	�B��B��B		�B	-)B	S@B	^5B	{�B	�oB	��B	��B	�{B	��B	�?B	��B	�dB	�6B	�JB	��B	�XB	��B	�B	�B	��B	�YB	��B	�HB	�"B	��B	��B	�pB	��B	�bB	��B	�$B	�OB	��B	�mB	�B	��B	��B	��B	�PB	�(B	��B	�aB	��B	�=B	ʌB	��B	��B	��B	�gB	՛B	��B	�gB	�4B	�SB	چB	�B	҉B	�sB	��B	߾B	��B	�bB	��B	�TB	�B	��B	��B	�B	�nB	�B	�B	�B	бB	οB	�dB	ȚB	�B	ĜB	�B	��B	�;B	��B	�yB	�DB	�B	�8B	�2B	�B	�,B	��B	�fB	�ZB	�eB	��B	�HB	�	B	��B	�DB	��B	�_B	�KB	��B	�B	�	B	��B	�B	�4B	�B	��B	��B	��B	��B	�2B	�zB	��B	��B	��B	�fB	�8B	��B	��B	�LB	��B	�RB	�RB	�	B	�xB	��B	�HB	�B	��B	�8B	�B	��B	�KB	��B	��B	�:B	�tB	�B	�B	�B	�B	��B	�BB	��B	ߊB	�;B	ݲB	�/B	ݲB	�5B	�5B	޸B	�BB	��B	��B	�&B	�zB	��B	�B	�B	�=B	�qB	�B	��B	�B	�B	�$B	�B	��B	��B	�8B	�qB	�=B	�5B	�vB	�!B	�B	�cB	�cB	�B	�3B	�B	��B	�B	��B	�`B	��B	�FB	��B	�>B	��B	��B	��B	��B	��B	�B	�(B	�]B	�wB	��B	�(B	�jB	��B	�(B	�.B	��B	��B
 OB
 OB
 �B
 4B
 �B
�B
B
�B
GB
�B
3B
�B
�B
�B
�B
�B
B
�B
MB
gB
MB
{B
�B
�B
�B
B
-B
{B
�B
�B
MB
B
gB
�B
�B
B
9B
9B
9B
B
�B
�B
�B
�B
�B
MB
gB
�B
�B
MB
�B
�B
gB
B
�B
�B
�B
�B
B
B
�B
�B
�B
�B
B
�B
B
fB
KB
�B
�B
�B
	lB
	RB
	RB
	7B
	7B
	7B
	�B

=B

	B

#B

rB

�B

�B
�B

�B

#B
	7B
_B
1B
�B
�B
KB
1B
fB
�B
	�B
6B
BB
"B
�B
�B
JB
�B
�B
�B
�B
�B
�B
vB
\B
BB
BB
vB
bB
B
hB
NB
 B
 B
4B
hB
hB
�B
B
TB
�B
�B
�B
B
B
&B
�B
�B
�B
�B
gB
�B
�B
mB
mB
mB
mB
mB
�B
�B
�B

B
$B
�B
�B
+B
�B
B
�B
_B
�B
)B
�B
=B
�B
�B
�B
�B
dB
IB
�B
�B
CB
�B
�B
�B
�B
/B
�B
B
B
�B
�B
�B
 B
 �B
 �B
 �B
 �B
 �B
 �B
 �B
 �B
!HB
!�B
"�B
#B
#:B
$&B
$�B
%B
%,B
%�B
&�B
(
B
(sB
)_B
*�B
+QB
+�B
+�B
+�B
,"B
,�B
,�B
,qB
,�B
-B
-wB
.IB
.�B
.�B
/OB
0!B
0UB
0�B
0�B
1'B
1B
0oB
0�B
2B
2-B
2GB
1vB
1[B
1[B
1[B
1�B
2-B
2�B
2�B
2�B
2�B
33B
3�B
3�B
3�B
4TB
4�B
4�B
5ZB
6zB
6�B
6�B
6�B
6�B
7�B
8RB
8�B
9$B
9XB
:B
:B
:B
:DB
:^B
:�B
;JB
;�B
;�B
;�B
;�B
<B
<jB
<6B
<6B
<�B
="B
="B
="B
=VB
=qB
=qB
=VB
<�B
<�B
<�B
<�B
=<B
>B
=�B
>(B
=�B
=B
=<B
=qB
=<B
=B
=B
<�B
<�B
=<B
=�B
=�B
=qB
=�B
=qB
=qB
=VB
=VB
=�B
=�B
=qB
<�B
<�B
=<B
="B
<�B
=B
="B
=�B
=�B
=�B
>BB
>�B
?}B
?cB
?}B
?�B
?�B
@�B
AUB
A�B
A�B
BB
BB
BAB
BAB
B�B
B�B
B�B
B�B
CaB
C{B
C{B
C�B
DB
DB
DMB
DgB
D�B
D�B
EB
EB
E9B
E�B
E�B
E�B
FB
F%B
F?B
F?B
FtB
F�B
F�B
G�B
G�B
HB
IB
I�B
I�B
J	B
J	B
I�B
J=B
JrB
J�B
J�B
KB
KDB
KxB
K�B
LB
L�B
L�B
MB
MB
MB
M�B
M�B
M�B
M�B
M�B
M�B
N"B
N<B
N�B
N�B
OBB
O�B
PB
O�B
P.B
PB
PB
PB
P�B
QB
QB
QB
QhB
Q�B
RB
R:B
R:B
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
UMB
U2B
UB
T�B
T�B
T�B
U2B
U2B
UB
U�B
UgB
U�B
U�B
U�B
V�B
V�B
V�B
W�B
W�B
W�B
X_B
XyB
XyB
YB
YKB
YB
Z7B
Z�B
[#B
[=B
[=B
[	B
[	B
[	B
[=B
[#B
[WB
[#B
[=B
[qB
[�B
[�B
[�B
[�B
\)B
\]B
]/B
]�B
]�B
]�B
^5B
^OB
^jB
^�B
^�B
_B
_VB
_�B
`BB
`�B
`�B
aB
aB
a-B
a-B
a-B
a-B
a|B
a�B
a�B
b�B
b�B
b�B
b�B
b�B
cTB
c�B
d@B
dZB
dtB
d�B
d�B
eB
eB
e,B
ezB
e�B
e�B
e�B
fB
fB
f2B
f�B
f�B
f�B
f�B
f�B
gB
gRB
g�B
h
B
hXB
hsB
h�B
i*B
i*B
iDB
iDB
i�B
i�B
i�B
jB
jKB
jeB
jB
jB
kB
kB
k6B
kQB
kQB
kQB
kQB
k�B
l=B
lWB
l�B
l�B
mB
m)B
l�B
mwB
nB
nB
n/B
ncB
n}B
n�B
o B
oOB
oOB
o�B
o�B
o�B
o�B
o�B
p!B
pUB
poB
p�B
p�B
p�B
p�B
qB
q'B
q'B
qvB
q�B
r�B
r�B
sB
s3B
sMB
shB
s�B
s�B
tB
tB
t9B
tTB
tnB
tnB
t�B
t�B
u%B
utB
uZB
u�B
u�B
u�B
vB
v+B
v�B
vzB
v�B
v�B
w2B
wLB
w�B
w�B
w�B
w�B
w�B
w�B
xB
xB
y	B
y>B
y>B
yXB
yrB
yrB
y�B
y�B
y�B
zB
y�B
y�B
y�B
y�B
y�B
y�B
y�B
z*B
zDB
z^B
zxB
zxB
zxB
z�B
z�B
{B
{dB
{�B
{�B
|6B
|6B
|B
|6B
|jB
|�B
|�B
|�B
}B
}<B
}�B
}�B
}�B
}�B
}�B
~(B
~]B
~]B
~�B
~�B
.B
H111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�rB�XB�=B�XB�=B�rB�XB�rBʌB��B��B��B��BʌB�XB�=B�XB�=B�XB�XB�XB�rBʌB��B��B�B��BʌB�~B�BB�B��B	5%B	qAB	xlB
d&B
\�B
raB
��B
�SB
�?B
��B
�~B
��B
�B
�-B!�BjB
�3BIlB��B�^BՁB��B��B�B�B�B<B�xB�B�$B�XB��B�"B�cB��BՁB�EBںB�aB��Bn�BSBP�B>wB4TB-�B&�B�B	B
��B
�OB
��B
�B
t�B
G_B
(sB	��B	�B	��B	��B	`\B	E�B	6+B	!|B	�B	
�B�;B�=B�RB��B�dB	-B	�B	B	0B	�B	,�B	T�B	P}B	YKB	hsB	RTB	9rB	/ B	# B	B	�B	pB	�B	�B	�B	�B��B��B		�B	-)B	S@B	^5B	{�B	�oB	��B	��B	�{B	��B	�?B	��B	�dB	�6B	�JB	��B	�XB	��B	�B	�B	��B	�YB	��B	�HB	�"B	��B	��B	�pB	��B	�bB	��B	�$B	�OB	��B	�mB	�B	��B	��B	��B	�PB	�(B	��B	�aB	��B	�=B	ʌB	��B	��B	��B	�gB	՛B	��B	�gB	�4B	�SB	چB	�B	҉B	�sB	��B	߾B	��B	�bB	��B	�TB	�B	��B	��B	�B	�nB	�B	�B	�B	бB	οB	�dB	ȚB	�B	ĜB	�B	��B	�;B	��B	�yB	�DB	�B	�8B	�2B	�B	�,B	��B	�fB	�ZB	�eB	��B	�HB	�	B	��B	�DB	��B	�_B	�KB	��B	�B	�	B	��B	�B	�4B	�B	��B	��B	��B	��B	�2B	�zB	��B	��B	��B	�fB	�8B	��B	��B	�LB	��B	�RB	�RB	�	B	�xB	��B	�HB	�B	��B	�8B	�B	��B	�KB	��B	��B	�:B	�tB	�B	�B	�B	�B	��B	�BB	��B	ߊB	�;B	ݲB	�/B	ݲB	�5B	�5B	޸B	�BB	��B	��B	�&B	�zB	��B	�B	�B	�=B	�qB	�B	��B	�B	�B	�$B	�B	��B	��B	�8B	�qB	�=B	�5B	�vB	�!B	�B	�cB	�cB	�B	�3B	�B	��B	�B	��B	�`B	��B	�FB	��B	�>B	��B	��B	��B	��B	��B	�B	�(B	�]B	�wB	��B	�(B	�jB	��B	�(B	�.B	��B	��B
 OB
 OB
 �B
 4B
 �B
�B
B
�B
GB
�B
3B
�B
�B
�B
�B
�B
B
�B
MB
gB
MB
{B
�B
�B
�B
B
-B
{B
�B
�B
MB
B
gB
�B
�B
B
9B
9B
9B
B
�B
�B
�B
�B
�B
MB
gB
�B
�B
MB
�B
�B
gB
B
�B
�B
�B
�B
B
B
�B
�B
�B
�B
B
�B
B
fB
KB
�B
�B
�B
	lB
	RB
	RB
	7B
	7B
	7B
	�B

=B

	B

#B

rB

�B

�B
�B

�B

#B
	7B
_B
1B
�B
�B
KB
1B
fB
�B
	�B
6B
BB
"B
�B
�B
JB
�B
�B
�B
�B
�B
�B
vB
\B
BB
BB
vB
bB
B
hB
NB
 B
 B
4B
hB
hB
�B
B
TB
�B
�B
�B
B
B
&B
�B
�B
�B
�B
gB
�B
�B
mB
mB
mB
mB
mB
�B
�B
�B

B
$B
�B
�B
+B
�B
B
�B
_B
�B
)B
�B
=B
�B
�B
�B
�B
dB
IB
�B
�B
CB
�B
�B
�B
�B
/B
�B
B
B
�B
�B
�B
 B
 �B
 �B
 �B
 �B
 �B
 �B
 �B
 �B
!HB
!�B
"�B
#B
#:B
$&B
$�B
%B
%,B
%�B
&�B
(
B
(sB
)_B
*�B
+QB
+�B
+�B
+�B
,"B
,�B
,�B
,qB
,�B
-B
-wB
.IB
.�B
.�B
/OB
0!B
0UB
0�B
0�B
1'B
1B
0oB
0�B
2B
2-B
2GB
1vB
1[B
1[B
1[B
1�B
2-B
2�B
2�B
2�B
2�B
33B
3�B
3�B
3�B
4TB
4�B
4�B
5ZB
6zB
6�B
6�B
6�B
6�B
7�B
8RB
8�B
9$B
9XB
:B
:B
:B
:DB
:^B
:�B
;JB
;�B
;�B
;�B
;�B
<B
<jB
<6B
<6B
<�B
="B
="B
="B
=VB
=qB
=qB
=VB
<�B
<�B
<�B
<�B
=<B
>B
=�B
>(B
=�B
=B
=<B
=qB
=<B
=B
=B
<�B
<�B
=<B
=�B
=�B
=qB
=�B
=qB
=qB
=VB
=VB
=�B
=�B
=qB
<�B
<�B
=<B
="B
<�B
=B
="B
=�B
=�B
=�B
>BB
>�B
?}B
?cB
?}B
?�B
?�B
@�B
AUB
A�B
A�B
BB
BB
BAB
BAB
B�B
B�B
B�B
B�B
CaB
C{B
C{B
C�B
DB
DB
DMB
DgB
D�B
D�B
EB
EB
E9B
E�B
E�B
E�B
FB
F%B
F?B
F?B
FtB
F�B
F�B
G�B
G�B
HB
IB
I�B
I�B
J	B
J	B
I�B
J=B
JrB
J�B
J�B
KB
KDB
KxB
K�B
LB
L�B
L�B
MB
MB
MB
M�B
M�B
M�B
M�B
M�B
M�B
N"B
N<B
N�B
N�B
OBB
O�B
PB
O�B
P.B
PB
PB
PB
P�B
QB
QB
QB
QhB
Q�B
RB
R:B
R:B
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
UMB
U2B
UB
T�B
T�B
T�B
U2B
U2B
UB
U�B
UgB
U�B
U�B
U�B
V�B
V�B
V�B
W�B
W�B
W�B
X_B
XyB
XyB
YB
YKB
YB
Z7B
Z�B
[#B
[=B
[=B
[	B
[	B
[	B
[=B
[#B
[WB
[#B
[=B
[qB
[�B
[�B
[�B
[�B
\)B
\]B
]/B
]�B
]�B
]�B
^5B
^OB
^jB
^�B
^�B
_B
_VB
_�B
`BB
`�B
`�B
aB
aB
a-B
a-B
a-B
a-B
a|B
a�B
a�B
b�B
b�B
b�B
b�B
b�B
cTB
c�B
d@B
dZB
dtB
d�B
d�B
eB
eB
e,B
ezB
e�B
e�B
e�B
fB
fB
f2B
f�B
f�B
f�B
f�B
f�B
gB
gRB
g�B
h
B
hXB
hsB
h�B
i*B
i*B
iDB
iDB
i�B
i�B
i�B
jB
jKB
jeB
jB
jB
kB
kB
k6B
kQB
kQB
kQB
kQB
k�B
l=B
lWB
l�B
l�B
mB
m)B
l�B
mwB
nB
nB
n/B
ncB
n}B
n�B
o B
oOB
oOB
o�B
o�B
o�B
o�B
o�B
p!B
pUB
poB
p�B
p�B
p�B
p�B
qB
q'B
q'B
qvB
q�B
r�B
r�B
sB
s3B
sMB
shB
s�B
s�B
tB
tB
t9B
tTB
tnB
tnB
t�B
t�B
u%B
utB
uZB
u�B
u�B
u�B
vB
v+B
v�B
vzB
v�B
v�B
w2B
wLB
w�B
w�B
w�B
w�B
w�B
w�B
xB
xB
y	B
y>B
y>B
yXB
yrB
yrB
y�B
y�B
y�B
zB
y�B
y�B
y�B
y�B
y�B
y�B
y�B
z*B
zDB
z^B
zxB
zxB
zxB
z�B
z�B
{B
{dB
{�B
{�B
|6B
|6B
|B
|6B
|jB
|�B
|�B
|�B
}B
}<B
}�B
}�B
}�B
}�B
}�B
~(B
~]B
~]B
~�B
~�B
.B
H111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104948  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604175035  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604175036  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604175036                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605025043  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605025043  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610141505                      G�O�G�O�G�O�                