CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:11:14Z creation;2022-06-04T19:11:14Z conversion to V3.1      
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
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p$   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �,   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �4   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �l   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ݜ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �(   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �,   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �<   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �@   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �D   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �HArgo profile    3.1 1.2 19500101000000  20220604191114  20220610151508  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @دx�d�1   @دyA��@04��E��d"��`B1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A   A@  A`  A�  A�33A�  A�  A�  A�  A�  A�  B   BffB  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�ffB���B���B�  B�  B�33B�  B�ffB�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  C   C  C  C  C  C
  C33C  C�fC  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6L�C7�fC9�fC<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb�Cd�Cf  Ch  Cj  Cl  Cn  Cp  Cr�Cs�fCv  Cw�fCy�fC|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� DfD�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'�fD(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @p�@s�
@��@��A��A<��A\��A|��A��A�z�A�z�A�z�A�z�A�z�A�z�A�z�B��B=qB=qB=qB'=qB/=qB7=qB?=qBG=qBO=qBW=qB_=qBg=qBo=qBw=qB=qB���B���B���B���B���B���B���B���B���B�B�8RB�k�B���B���B���B���B�BǞ�B˞�BϞ�BӞ�Bמ�B۞�Bߞ�B㞸B瞸B랸B�k�B�B���B���B���C�\C�\C�\C�\C	�\C�C�\C��C�\C�\C�\C�\C�\C�\C�\C�\C!�\C#�\C%�\C'�\C)�\C+�\C-�\C/�\C1�\C3�\C6)C7��C9��C;�\C=�\C?�\CA�\CC�\CE�\CG�\CI�\CK�\CM�\CO�\CQ�\CS�\CU�\CW�\CY�\C[�\C]�\C_�\Ca��Cc��Ce�\Cg�\Ci�\Ck�\Cm�\Co�\Cq��Cs��Cu�\Cw��Cy��C{�\C}�\C�\C��C��C��C��C��C��C��C��{C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��{C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��{C��{C��C��C��C��C��C��C��C��C��C��D s�D ��Ds�D��Ds�D��Dz=D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��D	s�D	��D
s�D
��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D�=Dz=D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��D s�D ��D!s�D!��D"s�D"��D#s�D#��D$s�D$��D%s�D%��D&s�D&��D'z=D'��D(s�D(��D)s�D)��D*s�D*��D+s�D+��D,s�D,��D-s�D-��D.s�D.��D/s�D/��D0s�D0��D1s�D1��D2s�D2��D3s�D3��D4s�D4��D5s�D5��D6s�D6��D7s�D7��D8s�D8��D9s�D9��D:s�D:��D;s�D;��D<s�D<��D=s�D=��D>s�D>��D?s�D?��D@s�D@��DAs�DA��DBs�DB��DCs�DC��DDs�DD��DEs�DE��DFs�DF��DGs�DG��DHs�DH��DIs�DI��DJs�DJ��DKs�DK��DLs�DL��DMs�DM��DNs�DN��DOs�DO��DPs�DP��DQs�DQ��DRs�DR��DSs�DS��DTs�DT��DUs�DU��DVs�DV��DWs�DW��DXs�DX��DYs�DY��DZs�DZ��D[s�D[��D\s�D\��D]s�D]��D^s�D^��D_s�D_��D`s�D`��Das�Da��Dbs�Db��Dcs�Dc��Dds�Dd��Des�De��Dfs�Df��Dgs�Dg��Dhs�Dh��Dis�Di��Djs�Dj��Dks�Dk��Dls�Dl��Dms�Dm��Dns�Dn��Dos�Do��Dps�Dp��Dqs�Dq��Drs�Dr��Dss�Ds��Dts�Dt��Dus�Du��Dvs�Dv��Dws�Dw��Dxs�Dx��Dys�Dy��Dzs�Dz��D{s�D{��D|s�D|��D}s�D}��D~s�D~��Ds�D��D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�}D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D��D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D¹�D���D�9�D�y�Dù�D���D�9�D�y�DĹ�D���D�9�D�y�DŹ�D���D�9�D�y�Dƹ�D���D�9�D�y�Dǹ�D���D�9�D�y�Dȹ�D���D�9�D�y�Dɹ�D���D�9�D�y�Dʹ�D���D�9�D�y�D˹�D���D�9�D�y�D̹�D���D�9�D�y�D͹�D���D�9�D�y�Dι�D���D�9�D�y�DϹ�D���D�9�D�y�Dй�D���D�9�D�y�Dѹ�D���D�9�D�y�Dҹ�D���D�9�D�y�Dӹ�D���D�9�D�y�DԹ�D���D�9�D�y�Dչ�D���D�9�D�y�Dֹ�D���D�9�D�y�D׹�D���D�9�D�y�Dع�D���D�9�D�y�Dٹ�D���D�9�D�y�Dڹ�D���D�9�D�y�D۹�D���D�9�D�y�Dܹ�D���D�9�D�y�Dݹ�D���D�9�D�y�D޹�D���D�9�D�y�D߹�D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D���D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D��R111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��A��|A��QA���A��aA�ϫA���A��^A�ѷAʾBAʶ�AʵtAʰ�Aʲ-Aʲ-Aʰ�Aʯ�AʯAʯAʪ�AʨXAʡ�Aʞ�Aʖ�Aʃ�A�GEA��GAɆ�A��A��A��A���A��A��(A�(A��A�A�A��A���A���Aȿ}A�ںA�v+A�	7AŹ$A�M�A��A��A�E�A�lWA�oA���A�cTA�%A���A���A���A��=A��jA�_A��zA���A��	A�HKA��A�.A��A��{A�K�A��6A�A�,�A�A�1A�[�A��A���A���A�5�A�ӏA��A�Y�A��[A��A��9AJ�Az[WAw]�As��An	�AlFtAf`�A_ĜA\�,AZ+�AX�ATɆAR_�ANQAJ|�AF�|AD��AB��A@��A?jA=a�A;�NA:(�A9��A:XyA:'�A9��A9(�A8�A8��A8aA7�sA8'RA8b�A8�A5�A/��A.�PA-��A-�A-y>A,��A+0UA*OA)�A)0�A(  A'�$A'�A&�@A%hsA$�XA$,=A#��A#�A#v`A#A"�A"�jA"��A!a|A ��A`BA!AxlAe,A,=A|�A��A�mAZA�A �A��A~(A��A	�A_pA�A�mA҉A��A�2A��A(A�[A�A��A:�A��A�A��A�jA,=A;dA~�A��A_�A�A�A�A	jA�HA�=AsAQA��Ap�A|A0�A\�A��A��A�A�)A�dAXyA�.Ai�A �$A #�@���@�#�@���@���@��R@��b@��,@��[@�}V@�$�@��@��@��@���@�+k@�~�@���@�F�@�o @�ݘ@�1@�5�@�4n@�(@��D@�J@���@�.I@��@�Dg@���@���@�}�@��T@��@�x@�V�@�p;@���@�?@��@��@��@�ں@��@�\�@�M�@�$�@ᦵ@ව@���@�{@�C-@�V@ߙ�@ߡ�@��@�M@��]@�IR@� i@�ߤ@�$t@�c�@ކY@��U@�e�@�Ta@�ϫ@�hs@ܱ�@ے:@�*0@ښ�@�@�U2@چY@��]@�l�@��@ؓu@ײ�@֖�@��@Ծ@�Ft@Ӡ'@���@Ѭq@��E@�Y@а�@�Z@��g@�C@�ߤ@�~(@Ͱ�@�J#@̰!@���@��2@�=q@���@��@Ȫe@Ȅ�@�	@�a@�ߤ@ƪe@�Ta@Ł�@�/�@č�@�$@ý�@�҉@���@��~@��|@���@�\�@��+@�o�@��y@�z�@�/�@��}@���@���@���@�c�@��m@�)_@��u@��0@���@�+@�@@�ی@���@��@���@�@���@�a�@���@�c @�8�@���@�f�@�U�@�8@��@���@���@��@��{@�&@���@���@�bN@��@�v`@�c�@�G�@�Ɇ@�oi@��@�  @���@�/@���@�h
@�&�@��@�ƨ@�l�@�4�@�(�@�S@���@��.@�PH@���@�u�@�RT@�F@�<6@�"�@��@��K@���@�-@��@�)_@���@�z�@�M@���@���@��'@��+@�6@��N@��V@�m]@�X@�$t@��@���@���@��\@�R�@��A@���@�8@���@���@�{�@�Ov@�4n@���@�%F@��@���@��Y@�(�@��@� �@�
�@��>@�RT@���@��@��f@��E@���@�I�@��C@�)_@��@�͟@�Q@��A@��
@���@��W@�	@��Q@�ƨ@��7@�k�@��@�ȴ@��o@��W@�{J@��@��,@���@��@��P@�^�@�?}@��@��j@�m�@�<�@�!@���@��X@�v`@�X�@�Dg@���@��@��@�g8@�V�@�A�@��@���@� \@���@���@��'@�~(@�-�@��@��r@��A@��
@��H@��~@�x�@���@���@�W�@���@��k@�qv@�dZ@�Vm@�E9@�=@��)@�h
@��@���@��h@��@��@�_�@�+k@�7@��+@��H@��@�[W@��@��!@�~(@�H�@�b@���@���@�:�@�ی@���@�_�@�J�@�$@��+@��g@���@���@�s�@�B�@��K@��@��D@�.�@�@��;@�Dg@� \@��@��@��1@��@�W�@�5?@��@���@�}�@�Q�@�<6@�+@��@��p@�z�@�Ov@�>B@��@P�@~l�@~e@}�Z@}��@}|@}7L@}-w@|�@|�4@{��@{P�@{J#@{K�@{>�@z�<@zO@y�o@y��@y�h@y0�@xtT@xI�@x7�@w�
@wdZ@v��@vs�@v
�@u�~@uA @t�@t2�@t�@s�@s��@s.I@r��@r�@q�#@q@q�'@q�@qS&@p��@pbN@o��@o�@n�x@n�@m`B@l��@lG@k��@kg�@k9�@k�@j�@j#:@i%@h�v@hr�@h�@gqv@f��@f��@f($@e�h@eA @e(�@d��@dK^@c�w@cJ#@cY@b�H@b��@a��@a��@ae,@aV@`h�@`7@_�@_�	@_x@_a@_A�@_"�@^�@^h
@]��@]\�@\�P@\�D@[��@[��@[H�@[�@Z��@Z($@Y@YX@YV@X��@X_@XU2@X	�@Wb�@WS�@W�@V��@V$�@U�'@U�@T��@T��@T`�@S�@S{J@SS�@R�H@R��@Q�o@Q�#@Q�S@Qs�@QA @P�@P��@P%�@O�[@O.I@N�@N�@NH�@N!�@N�@N�@M�@M��@MY�@L�/@L�@K��@K��@K~�@K>�@J�,@J��@Jl�@Jd�@J3�@I��@I�-@I�@Ic@I}�@Ix�@Io @IS&@Hl"@H1@G�0@G��@G�:@G_p@F�"@F�@F_�@F=q@FJ@E��@E��@E��@E�~@Ex�@Ec�@E+�@D��@D��@D�I@D�_@Dz�@DFt@D:�@D7�@D6@D �@DG@C�
@Cqv@C�@B��@B�x@B}V@Bc @B�@A2a@@�|@@��@@��@@��@@?�@@G@?�g@?��@?�@>��@>ں@>�\@=�@=��@=��@=T�@=�@<��@<	�@;��@;{J@;~�@;=@:��@:J�@9��@9Dg@8��@8u�@8<�@7�+@7��@7s@78@7S@6�@6͟@6��@6{@5��@5<6@4��@4�E@4�@47�@3�6@3�@2ߤ@2}V@1��@1��@1e,@1�@0�@0��@0z�@0_@0~@/l�@/�@.��@.@�@-�@-a�@,�@,7@+\)@*��@*�b@*�1@*u%@*E�@)�H@(�P@(�@(7@'�$@'b�@';d@'&@'S@&�]@&�x@&u%@&?@%�^@%T�@%5�@%0�@% \@$��@$��@$��@$��@$	�@#��@#n/@#=@"�s@"h
@"�@!��@!��@!��@![W@!-w@ ��@ ��@ I�@ 	�@��@�@�:@Mj@�@��@n�@ff@Z�@�@��@f�@-w@�@�@�f@�@�j@oi@?�@@��@��@�k@_p@Y@��@�@ff@#:@ϫ@�3@�H@��@��@Q�@<6@7L@2a@�@��@��@��@Z@A�@1'@/�@~@�&@�@�f@Z�@>�@�@͟@��@�A@a|@�@�C@c@`B@B�@#�@@�K@�@9X@�@�Q@y�@dZ@W?@X�@U�@K�@1�@!-@��@�h@�@�+@xl@h
@5?@�@�'@Y�@�@��@��@M@  @�@�q@�*@��@�	@s@a@P�@9�@'�@�@�@��@)�@��@��@�@hs@-w@V@�@�@�p@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��A��|A��QA���A��aA�ϫA���A��^A�ѷAʾBAʶ�AʵtAʰ�Aʲ-Aʲ-Aʰ�Aʯ�AʯAʯAʪ�AʨXAʡ�Aʞ�Aʖ�Aʃ�A�GEA��GAɆ�A��A��A��A���A��A��(A�(A��A�A�A��A���A���Aȿ}A�ںA�v+A�	7AŹ$A�M�A��A��A�E�A�lWA�oA���A�cTA�%A���A���A���A��=A��jA�_A��zA���A��	A�HKA��A�.A��A��{A�K�A��6A�A�,�A�A�1A�[�A��A���A���A�5�A�ӏA��A�Y�A��[A��A��9AJ�Az[WAw]�As��An	�AlFtAf`�A_ĜA\�,AZ+�AX�ATɆAR_�ANQAJ|�AF�|AD��AB��A@��A?jA=a�A;�NA:(�A9��A:XyA:'�A9��A9(�A8�A8��A8aA7�sA8'RA8b�A8�A5�A/��A.�PA-��A-�A-y>A,��A+0UA*OA)�A)0�A(  A'�$A'�A&�@A%hsA$�XA$,=A#��A#�A#v`A#A"�A"�jA"��A!a|A ��A`BA!AxlAe,A,=A|�A��A�mAZA�A �A��A~(A��A	�A_pA�A�mA҉A��A�2A��A(A�[A�A��A:�A��A�A��A�jA,=A;dA~�A��A_�A�A�A�A	jA�HA�=AsAQA��Ap�A|A0�A\�A��A��A�A�)A�dAXyA�.Ai�A �$A #�@���@�#�@���@���@��R@��b@��,@��[@�}V@�$�@��@��@��@���@�+k@�~�@���@�F�@�o @�ݘ@�1@�5�@�4n@�(@��D@�J@���@�.I@��@�Dg@���@���@�}�@��T@��@�x@�V�@�p;@���@�?@��@��@��@�ں@��@�\�@�M�@�$�@ᦵ@ව@���@�{@�C-@�V@ߙ�@ߡ�@��@�M@��]@�IR@� i@�ߤ@�$t@�c�@ކY@��U@�e�@�Ta@�ϫ@�hs@ܱ�@ے:@�*0@ښ�@�@�U2@چY@��]@�l�@��@ؓu@ײ�@֖�@��@Ծ@�Ft@Ӡ'@���@Ѭq@��E@�Y@а�@�Z@��g@�C@�ߤ@�~(@Ͱ�@�J#@̰!@���@��2@�=q@���@��@Ȫe@Ȅ�@�	@�a@�ߤ@ƪe@�Ta@Ł�@�/�@č�@�$@ý�@�҉@���@��~@��|@���@�\�@��+@�o�@��y@�z�@�/�@��}@���@���@���@�c�@��m@�)_@��u@��0@���@�+@�@@�ی@���@��@���@�@���@�a�@���@�c @�8�@���@�f�@�U�@�8@��@���@���@��@��{@�&@���@���@�bN@��@�v`@�c�@�G�@�Ɇ@�oi@��@�  @���@�/@���@�h
@�&�@��@�ƨ@�l�@�4�@�(�@�S@���@��.@�PH@���@�u�@�RT@�F@�<6@�"�@��@��K@���@�-@��@�)_@���@�z�@�M@���@���@��'@��+@�6@��N@��V@�m]@�X@�$t@��@���@���@��\@�R�@��A@���@�8@���@���@�{�@�Ov@�4n@���@�%F@��@���@��Y@�(�@��@� �@�
�@��>@�RT@���@��@��f@��E@���@�I�@��C@�)_@��@�͟@�Q@��A@��
@���@��W@�	@��Q@�ƨ@��7@�k�@��@�ȴ@��o@��W@�{J@��@��,@���@��@��P@�^�@�?}@��@��j@�m�@�<�@�!@���@��X@�v`@�X�@�Dg@���@��@��@�g8@�V�@�A�@��@���@� \@���@���@��'@�~(@�-�@��@��r@��A@��
@��H@��~@�x�@���@���@�W�@���@��k@�qv@�dZ@�Vm@�E9@�=@��)@�h
@��@���@��h@��@��@�_�@�+k@�7@��+@��H@��@�[W@��@��!@�~(@�H�@�b@���@���@�:�@�ی@���@�_�@�J�@�$@��+@��g@���@���@�s�@�B�@��K@��@��D@�.�@�@��;@�Dg@� \@��@��@��1@��@�W�@�5?@��@���@�}�@�Q�@�<6@�+@��@��p@�z�@�Ov@�>B@��@P�@~l�@~e@}�Z@}��@}|@}7L@}-w@|�@|�4@{��@{P�@{J#@{K�@{>�@z�<@zO@y�o@y��@y�h@y0�@xtT@xI�@x7�@w�
@wdZ@v��@vs�@v
�@u�~@uA @t�@t2�@t�@s�@s��@s.I@r��@r�@q�#@q@q�'@q�@qS&@p��@pbN@o��@o�@n�x@n�@m`B@l��@lG@k��@kg�@k9�@k�@j�@j#:@i%@h�v@hr�@h�@gqv@f��@f��@f($@e�h@eA @e(�@d��@dK^@c�w@cJ#@cY@b�H@b��@a��@a��@ae,@aV@`h�@`7@_�@_�	@_x@_a@_A�@_"�@^�@^h
@]��@]\�@\�P@\�D@[��@[��@[H�@[�@Z��@Z($@Y@YX@YV@X��@X_@XU2@X	�@Wb�@WS�@W�@V��@V$�@U�'@U�@T��@T��@T`�@S�@S{J@SS�@R�H@R��@Q�o@Q�#@Q�S@Qs�@QA @P�@P��@P%�@O�[@O.I@N�@N�@NH�@N!�@N�@N�@M�@M��@MY�@L�/@L�@K��@K��@K~�@K>�@J�,@J��@Jl�@Jd�@J3�@I��@I�-@I�@Ic@I}�@Ix�@Io @IS&@Hl"@H1@G�0@G��@G�:@G_p@F�"@F�@F_�@F=q@FJ@E��@E��@E��@E�~@Ex�@Ec�@E+�@D��@D��@D�I@D�_@Dz�@DFt@D:�@D7�@D6@D �@DG@C�
@Cqv@C�@B��@B�x@B}V@Bc @B�@A2a@@�|@@��@@��@@��@@?�@@G@?�g@?��@?�@>��@>ں@>�\@=�@=��@=��@=T�@=�@<��@<	�@;��@;{J@;~�@;=@:��@:J�@9��@9Dg@8��@8u�@8<�@7�+@7��@7s@78@7S@6�@6͟@6��@6{@5��@5<6@4��@4�E@4�@47�@3�6@3�@2ߤ@2}V@1��@1��@1e,@1�@0�@0��@0z�@0_@0~@/l�@/�@.��@.@�@-�@-a�@,�@,7@+\)@*��@*�b@*�1@*u%@*E�@)�H@(�P@(�@(7@'�$@'b�@';d@'&@'S@&�]@&�x@&u%@&?@%�^@%T�@%5�@%0�@% \@$��@$��@$��@$��@$	�@#��@#n/@#=@"�s@"h
@"�@!��@!��@!��@![W@!-w@ ��@ ��@ I�@ 	�@��@�@�:@Mj@�@��@n�@ff@Z�@�@��@f�@-w@�@�@�f@�@�j@oi@?�@@��@��@�k@_p@Y@��@�@ff@#:@ϫ@�3@�H@��@��@Q�@<6@7L@2a@�@��@��@��@Z@A�@1'@/�@~@�&@�@�f@Z�@>�@�@͟@��@�A@a|@�@�C@c@`B@B�@#�@@�K@�@9X@�@�Q@y�@dZ@W?@X�@U�@K�@1�@!-@��@�h@�@�+@xl@h
@5?@�@�'@Y�@�@��@��@M@  @�@�q@�*@��@�	@s@a@P�@9�@'�@�@�@��@)�@��@��@�@hs@-w@V@�@�@�p@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BBVB�B�B�B�B�B�B�B�B�B"B�B�B�BBBBBBBBBbBhB�B�B2BCB&B1�B@OBB�BDgBEBF�Bl�B}�BȚB��B	&2B	`BB	l�B	z�B	��B	�B
?cB
`\B
c�B
��B
��B
�<B�#B�aB~�B|PB~Bm�B
��B�B�BBB vB
��B
��B
�B
�PB
��B
��B<PB8�B�B
��B
��B
�"B
��B
��B
�^B
wfB
dZB
V�B
L�B
Z�B
a|B
8B
.IB
$&B
1B	�cB	�@B	�fB	�FB	��B	��B	Z7B	:*B	&�B	�B	�B�B��B��B��B��B�[B�VB�jB�PB�iB�#B�\B��B�nB	�B	#B	#B	#�B	&2B	)B	>]B	X�B	��B	�lB	��B	w�B	m�B	tB	��B	�mB	~�B	s�B	o�B	raB	|�B	��B	��B	��B	��B	��B	�)B	��B	�9B	�RB	��B	�B	�3B	��B	�nB	�!B	��B	��B	��B	�B	�3B	��B	�(B	�6B	��B	|�B	v�B	q'B	lB	lqB	h�B	g�B	r-B	i�B	a�B	u�B	��B	�B	v+B	jB	h>B	g8B	hXB	pB	}�B	�B	�B	�OB	z�B	� B	��B	�vB	��B	�JB	�#B	�EB	|�B	w�B	v�B	w�B	x�B	u%B	s�B	n/B	^jB	WYB	T�B	f2B	�MB	��B	�(B	��B	��B	�B	�B	z�B	x�B	x�B	t�B	~B	HB	�oB	�&B	��B	��B	��B	�>B	��B	�-B	��B	�kB	��B	��B	��B	�B	��B	�3B	�[B	��B	��B	�3B	��B	��B	�6B	��B	��B	��B	��B	�aB	��B	�B	��B	��B	��B	��B	�xB	��B	��B	�jB	��B	��B	��B	�HB	��B	�.B	�VB	��B	�DB	�6B	��B	�B	�)B	�5B	�2B	��B	�vB	��B	�nB	��B	ðB	ȴB	�6B	��B	�bB	�NB	�4B	�bB	�<B	�PB	̳B	̳B	��B	��B	��B	�B	�EB	�SB	��B	�BB	͟B	��B	�B	��B	ˬB	�xB	�0B	��B	��B	�sB	�EB	�KB	�KB	ٚB	�kB	�B	��B	�eB	�B	��B	��B	��B	�jB	޸B	��B	޸B	ߊB	�pB	߾B	��B	�B	��B	�B	��B	��B	� B	�B	�&B	��B	�`B	�B	�B	��B	�B	�B	�B	�B	�
B	��B	�_B	��B	�QB	�B	�B	�eB	�0B	�KB	�eB	��B	��B	��B	�B	��B	��B	�eB	�KB	�KB	�B	�B	��B	��B	�B	�B	�B	�)B	�wB	�B	�B	�B	��B	�B	�;B	�!B	�!B	��B	�'B	�[B	��B	�AB	�B	�B	�3B	�B	��B	��B	��B	��B	��B	�B	�tB	��B	��B	�`B	�B	�LB	�2B	�2B	�LB	�2B	��B	��B	�8B	�8B	��B	�	B	�$B	�	B	��B	��B	�B	�JB	��B	��B	�jB	�PB	��B	��B	�<B	�"B	�"B	�<B	�<B	��B
 iB	��B	��B	��B
UB
;B
;B
�B
B
�B
�B
B
�B
�B
�B
�B
SB
B
�B
tB
�B
+B
�B
�B
+B
�B
�B
YB
%B
+B
�B
^B
�B
vB
�B
�B
NB
�B
�B
 B
oB
�B
�B
{B
aB
�B
�B
B
gB
�B
B
mB
�B
9B
9B
SB
SB
9B
�B
�B
�B
MB
2B
MB
2B
B
B
gB
�B
�B
�B
�B
B
9B
�B
�B
�B
�B
�B
�B
_B
�B
EB
_B
_B
EB
yB
eB
7B
7B
�B
�B
#B
qB
B
�B
IB
�B
�B
�B
�B
�B
B
B
�B
 'B
 �B
 �B
 �B
 �B
 \B
 �B
!B
!|B
!�B
!�B
"B
"NB
"4B
"�B
#B
#B
#:B
$&B
%B
%zB
%�B
&�B
'RB
'�B
)B
)DB
)DB
)�B
)�B
)�B
)�B
)�B
*eB
+6B
+�B
,B
,B
,B
,"B
,�B
-wB
-�B
-�B
.B
.}B
/�B
/�B
/�B
/�B
/�B
0!B
0B
0B
0B
0�B
1B
0�B
0�B
0�B
1AB
1�B
1�B
2B
2B
2-B
2�B
2�B
2�B
2�B
2�B
2�B
2�B
3�B
3�B
3hB
4�B
4�B
5B
5�B
5�B
6FB
8B
9	B
9	B
9$B
9rB
9�B
:DB
:�B
:�B
;JB
;�B
<B
<6B
<PB
<jB
=�B
=�B
>B
>B
>(B
>B
>�B
?}B
?.B
?cB
?cB
@B
@B
@OB
@�B
A B
A B
@�B
A B
A�B
BB
B[B
B[B
B[B
B�B
B�B
B�B
C-B
C-B
C�B
C�B
D3B
DgB
DgB
D�B
D�B
DgB
D�B
EB
ESB
E�B
E�B
E�B
EmB
EmB
ESB
ESB
F?B
F�B
GEB
G�B
G�B
H�B
IB
H�B
IRB
I�B
I�B
I�B
I�B
J	B
J�B
J�B
J�B
J�B
J�B
KDB
K^B
KxB
K�B
L0B
L�B
M6B
MjB
MjB
M�B
M�B
M�B
M�B
N"B
NpB
N�B
N�B
OB
N�B
N�B
N�B
N�B
N�B
N�B
OB
OB
O\B
OvB
O�B
O�B
P.B
P}B
P}B
PbB
PbB
PbB
PbB
PHB
PHB
PHB
PHB
P.B
O�B
Q B
QNB
Q�B
Q�B
Q�B
QhB
Q B
P}B
PHB
P�B
QB
Q4B
QNB
Q�B
Q�B
Q�B
Q�B
Q�B
RoB
RTB
RTB
RTB
R�B
R�B
R�B
R�B
R�B
S&B
S&B
S[B
S�B
SuB
SuB
S�B
S[B
S[B
S@B
TFB
T{B
T�B
U2B
UB
UgB
U�B
U�B
U�B
U�B
UMB
UB
UB
U�B
U�B
U�B
VSB
V9B
VB
V�B
W
B
W$B
W?B
W�B
W�B
WsB
W�B
W�B
W�B
XB
XB
XEB
X�B
X�B
Y1B
YB
Y�B
YB
Y�B
ZQB
Z�B
Z�B
Z�B
Z�B
Z�B
[=B
[=B
[�B
[�B
[�B
\xB
\�B
\�B
]dB
]~B
]�B
]�B
]�B
]�B
^jB
^�B
_B
_B
_;B
_�B
`'B
`vB
aB
a|B
a�B
a�B
a�B
a�B
bB
b�B
b�B
cTB
c�B
dB
d&B
d&B
d@B
dZB
d�B
d�B
d�B
eFB
e�B
e�B
e�B
e�B
e�B
f2B
fB
ffB
f�B
gB
gRB
gmB
g�B
h
B
hXB
h�B
h�B
h�B
h�B
h�B
i*B
i_B
i�B
i�B
i�B
j0B
jKB
jB
j�B
kB
k6B
kB
k6B
k�B
k�B
lB
l"B
lWB
l=B
lWB
lWB
lqB
l�B
l�B
mB
m)B
mCB
m]B
m�B
m�B
m�B
nB
nIB
ncB
n�B
n�B
n�B
n�B
n�B
o B
o B
oB
oB
o5B
oOB
o�B
o�B
o�B
o�B
o�B
o�B
o�B
p!B
p;B
pUB
p�B
p�B
p�B
p�B
qB
q'B
q'B
q�B
q�B
q�B
rB
r-B
raB
r|B
r|B
r�B
sB
sMB
s�B
tB
s�B
tB
tB
tB
tB
t9B
t9B
tnB
t�B
t�B
t�B
t�B
t�B
t�B
uB
uZB
u�B
u�B
v+B
v`B
vzB
v�B
w2B
wfB
wLB
w�B
wfB
w�B
w�B
w�B
w�B
w�B
xB
x8B
y	B
y$B
y�B
y�B
y�B
y�B
zB
z*B
zDB
zDB
zxB
z�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BBVB�B�B�B�B�B�B�B�B�B"B�B�B�BBBBBBBBBbBhB�B�B2BCB&B1�B@OBB�BDgBEBF�Bl�B}�BȚB��B	&2B	`BB	l�B	z�B	��B	�B
?cB
`\B
c�B
��B
��B
�<B�#B�aB~�B|PB~Bm�B
��B�B�BBB vB
��B
��B
�B
�PB
��B
��B<PB8�B�B
��B
��B
�"B
��B
��B
�^B
wfB
dZB
V�B
L�B
Z�B
a|B
8B
.IB
$&B
1B	�cB	�@B	�fB	�FB	��B	��B	Z7B	:*B	&�B	�B	�B�B��B��B��B��B�[B�VB�jB�PB�iB�#B�\B��B�nB	�B	#B	#B	#�B	&2B	)B	>]B	X�B	��B	�lB	��B	w�B	m�B	tB	��B	�mB	~�B	s�B	o�B	raB	|�B	��B	��B	��B	��B	��B	�)B	��B	�9B	�RB	��B	�B	�3B	��B	�nB	�!B	��B	��B	��B	�B	�3B	��B	�(B	�6B	��B	|�B	v�B	q'B	lB	lqB	h�B	g�B	r-B	i�B	a�B	u�B	��B	�B	v+B	jB	h>B	g8B	hXB	pB	}�B	�B	�B	�OB	z�B	� B	��B	�vB	��B	�JB	�#B	�EB	|�B	w�B	v�B	w�B	x�B	u%B	s�B	n/B	^jB	WYB	T�B	f2B	�MB	��B	�(B	��B	��B	�B	�B	z�B	x�B	x�B	t�B	~B	HB	�oB	�&B	��B	��B	��B	�>B	��B	�-B	��B	�kB	��B	��B	��B	�B	��B	�3B	�[B	��B	��B	�3B	��B	��B	�6B	��B	��B	��B	��B	�aB	��B	�B	��B	��B	��B	��B	�xB	��B	��B	�jB	��B	��B	��B	�HB	��B	�.B	�VB	��B	�DB	�6B	��B	�B	�)B	�5B	�2B	��B	�vB	��B	�nB	��B	ðB	ȴB	�6B	��B	�bB	�NB	�4B	�bB	�<B	�PB	̳B	̳B	��B	��B	��B	�B	�EB	�SB	��B	�BB	͟B	��B	�B	��B	ˬB	�xB	�0B	��B	��B	�sB	�EB	�KB	�KB	ٚB	�kB	�B	��B	�eB	�B	��B	��B	��B	�jB	޸B	��B	޸B	ߊB	�pB	߾B	��B	�B	��B	�B	��B	��B	� B	�B	�&B	��B	�`B	�B	�B	��B	�B	�B	�B	�B	�
B	��B	�_B	��B	�QB	�B	�B	�eB	�0B	�KB	�eB	��B	��B	��B	�B	��B	��B	�eB	�KB	�KB	�B	�B	��B	��B	�B	�B	�B	�)B	�wB	�B	�B	�B	��B	�B	�;B	�!B	�!B	��B	�'B	�[B	��B	�AB	�B	�B	�3B	�B	��B	��B	��B	��B	��B	�B	�tB	��B	��B	�`B	�B	�LB	�2B	�2B	�LB	�2B	��B	��B	�8B	�8B	��B	�	B	�$B	�	B	��B	��B	�B	�JB	��B	��B	�jB	�PB	��B	��B	�<B	�"B	�"B	�<B	�<B	��B
 iB	��B	��B	��B
UB
;B
;B
�B
B
�B
�B
B
�B
�B
�B
�B
SB
B
�B
tB
�B
+B
�B
�B
+B
�B
�B
YB
%B
+B
�B
^B
�B
vB
�B
�B
NB
�B
�B
 B
oB
�B
�B
{B
aB
�B
�B
B
gB
�B
B
mB
�B
9B
9B
SB
SB
9B
�B
�B
�B
MB
2B
MB
2B
B
B
gB
�B
�B
�B
�B
B
9B
�B
�B
�B
�B
�B
�B
_B
�B
EB
_B
_B
EB
yB
eB
7B
7B
�B
�B
#B
qB
B
�B
IB
�B
�B
�B
�B
�B
B
B
�B
 'B
 �B
 �B
 �B
 �B
 \B
 �B
!B
!|B
!�B
!�B
"B
"NB
"4B
"�B
#B
#B
#:B
$&B
%B
%zB
%�B
&�B
'RB
'�B
)B
)DB
)DB
)�B
)�B
)�B
)�B
)�B
*eB
+6B
+�B
,B
,B
,B
,"B
,�B
-wB
-�B
-�B
.B
.}B
/�B
/�B
/�B
/�B
/�B
0!B
0B
0B
0B
0�B
1B
0�B
0�B
0�B
1AB
1�B
1�B
2B
2B
2-B
2�B
2�B
2�B
2�B
2�B
2�B
2�B
3�B
3�B
3hB
4�B
4�B
5B
5�B
5�B
6FB
8B
9	B
9	B
9$B
9rB
9�B
:DB
:�B
:�B
;JB
;�B
<B
<6B
<PB
<jB
=�B
=�B
>B
>B
>(B
>B
>�B
?}B
?.B
?cB
?cB
@B
@B
@OB
@�B
A B
A B
@�B
A B
A�B
BB
B[B
B[B
B[B
B�B
B�B
B�B
C-B
C-B
C�B
C�B
D3B
DgB
DgB
D�B
D�B
DgB
D�B
EB
ESB
E�B
E�B
E�B
EmB
EmB
ESB
ESB
F?B
F�B
GEB
G�B
G�B
H�B
IB
H�B
IRB
I�B
I�B
I�B
I�B
J	B
J�B
J�B
J�B
J�B
J�B
KDB
K^B
KxB
K�B
L0B
L�B
M6B
MjB
MjB
M�B
M�B
M�B
M�B
N"B
NpB
N�B
N�B
OB
N�B
N�B
N�B
N�B
N�B
N�B
OB
OB
O\B
OvB
O�B
O�B
P.B
P}B
P}B
PbB
PbB
PbB
PbB
PHB
PHB
PHB
PHB
P.B
O�B
Q B
QNB
Q�B
Q�B
Q�B
QhB
Q B
P}B
PHB
P�B
QB
Q4B
QNB
Q�B
Q�B
Q�B
Q�B
Q�B
RoB
RTB
RTB
RTB
R�B
R�B
R�B
R�B
R�B
S&B
S&B
S[B
S�B
SuB
SuB
S�B
S[B
S[B
S@B
TFB
T{B
T�B
U2B
UB
UgB
U�B
U�B
U�B
U�B
UMB
UB
UB
U�B
U�B
U�B
VSB
V9B
VB
V�B
W
B
W$B
W?B
W�B
W�B
WsB
W�B
W�B
W�B
XB
XB
XEB
X�B
X�B
Y1B
YB
Y�B
YB
Y�B
ZQB
Z�B
Z�B
Z�B
Z�B
Z�B
[=B
[=B
[�B
[�B
[�B
\xB
\�B
\�B
]dB
]~B
]�B
]�B
]�B
]�B
^jB
^�B
_B
_B
_;B
_�B
`'B
`vB
aB
a|B
a�B
a�B
a�B
a�B
bB
b�B
b�B
cTB
c�B
dB
d&B
d&B
d@B
dZB
d�B
d�B
d�B
eFB
e�B
e�B
e�B
e�B
e�B
f2B
fB
ffB
f�B
gB
gRB
gmB
g�B
h
B
hXB
h�B
h�B
h�B
h�B
h�B
i*B
i_B
i�B
i�B
i�B
j0B
jKB
jB
j�B
kB
k6B
kB
k6B
k�B
k�B
lB
l"B
lWB
l=B
lWB
lWB
lqB
l�B
l�B
mB
m)B
mCB
m]B
m�B
m�B
m�B
nB
nIB
ncB
n�B
n�B
n�B
n�B
n�B
o B
o B
oB
oB
o5B
oOB
o�B
o�B
o�B
o�B
o�B
o�B
o�B
p!B
p;B
pUB
p�B
p�B
p�B
p�B
qB
q'B
q'B
q�B
q�B
q�B
rB
r-B
raB
r|B
r|B
r�B
sB
sMB
s�B
tB
s�B
tB
tB
tB
tB
t9B
t9B
tnB
t�B
t�B
t�B
t�B
t�B
t�B
uB
uZB
u�B
u�B
v+B
v`B
vzB
v�B
w2B
wfB
wLB
w�B
wfB
w�B
w�B
w�B
w�B
w�B
xB
x8B
y	B
y$B
y�B
y�B
y�B
y�B
zB
z*B
zDB
zDB
zxB
z�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105226  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604191114  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604191114  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604191114                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605041122  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605041122  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610151508                      G�O�G�O�G�O�                