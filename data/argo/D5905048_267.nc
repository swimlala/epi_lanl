CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-08-03T00:35:25Z creation;2018-08-03T00:35:30Z conversion to V3.1;2019-12-19T07:32:06Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7T   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     88   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8X   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8\   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8d   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8h   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8p   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8x   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ID   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M0   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  pl   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tX   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �d   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20180803003525  20200116231517  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL              A   JA  I2_0577_267                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @�v�a�Q 1   @�v�u� @4y���d_���v1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   A   A>ffA`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B ffB(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D���D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @z=q@��@��A��A;\)A\��A|��A�z�A�z�A�z�A�z�A�z�A�z�A�z�A�z�B=qB=qB=qB��B'=qB/=qB7=qB?=qBG=qBO=qBW=qB_=qBg=qBo=qBw=qB=qB���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���BÞ�BǞ�B˞�BϞ�BӞ�Bמ�B۞�Bߞ�B㞸B瞸B랸BB�B���B���B���C�\C�\C�\C�\C	�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C!�\C#�\C%�\C'�\C)�\C+�\C-�\C/�\C1�\C3�\C5�\C7�\C9�\C;�\C=�\C?�\CA�\CC�\CE�\CG�\CI�\CK�\CM�\CO�\CQ�\CS�\CU�\CW�\CY�\C[�\C]�\C_�\Ca�\Cc�\Ce�\Cg�\Ci�\Ck�\Cm�\Co�\Cq�\Cs�\Cu�\Cw�\Cy�\C{�\C}�\C�\C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D s�D ��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��D	s�D	��D
s�D
��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D�qDs�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��D s�D ��D!s�D!��D"s�D"��D#s�D#��D$s�D$��D%s�D%��D&s�D&��D's�D'��D(s�D(��D)s�D)��D*s�D*��D+s�D+��D,s�D,��D-s�D-��D.s�D.��D/s�D/��D0s�D0��D1s�D1��D2s�D2��D3s�D3��D4s�D4��D5s�D5��D6s�D6��D7s�D7��D8s�D8��D9s�D9��D:s�D:��D;s�D;��D<s�D<��D=s�D=��D>s�D>��D?s�D?��D@s�D@��DAs�DA��DBs�DB��DCs�DC��DDs�DD��DEs�DE��DFs�DF��DGs�DG��DHs�DH��DIs�DI��DJs�DJ��DKs�DK��DLs�DL��DMs�DM��DNs�DN��DOs�DO��DPs�DP��DQs�DQ��DRs�DR��DSs�DS��DTs�DT��DUs�DU��DVs�DV��DWs�DW��DXs�DX��DYs�DY��DZs�DZ��D[s�D[��D\s�D\��D]s�D]��D^s�D^��D_s�D_��D`s�D`��Das�Da��Dbs�Db��Dcs�Dc��Dds�Dd��Des�De��Dfs�Df��Dgs�Dg��Dhs�Dh��Dis�Di��Djs�Dj��Dks�Dk��Dls�Dl��Dms�Dm��Dns�Dn��Dos�Do��Dps�Dp��Dqs�Dq��Drs�Dr��Dss�Ds��Dts�Dt��Dus�Du��Dvs�Dv��Dws�Dw��Dxs�Dx��Dys�Dy��Dzs�Dz��D{s�D{��D|s�D|��D}s�D}��D~s�D~��Ds�D��D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�6�D�v�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D¹�D���D�9�D�y�Dù�D���D�9�D�y�DĹ�D���D�9�D�y�DŹ�D���D�9�D�y�Dƹ�D���D�9�D�y�Dǹ�D���D�9�D�y�Dȹ�D���D�9�D�y�Dɹ�D���D�9�D�y�Dʹ�D���D�9�D�y�D˹�D���D�9�D�y�D̹�D���D�9�D�y�D͹�D���D�9�D�y�Dι�D���D�9�D�y�DϹ�D���D�9�D�y�Dй�D���D�9�D�y�Dѹ�D���D�9�D�y�Dҹ�D���D�9�D�y�Dӹ�D���D�9�D�y�DԹ�D���D�9�D�y�Dչ�D���D�9�D�y�Dֹ�D���D�9�D�y�D׹�D���D�9�D�y�Dع�D���D�9�D�y�Dٹ�D���D�9�D�y�Dڹ�D���D�9�D�y�D۹�D���D�9�D�y�Dܹ�D���D�9�D�y�Dݹ�D���D�9�D�y�D޹�D���D�9�D�y�D߹�D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�6�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�}D��D���D�9�D�y�D��D���D�9�D�y�D���D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D���D��R111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Aݡ�Aݡ�Aݟ�Aݣ�Aݟ�Aݝ�Aݝ�Aݙ�Aݛ�Aݝ�Aݟ�Aݟ�Aݟ�Aݡ�Aݡ�Aݛ�A�|�A�ffA�$�AܼjA�$�A۲-A�oA�$�A��A�v�AӅA�M�A�M�A��A̧�A���A��HA�E�A�1'A�|�Aƺ^A��Aİ!A���A�v�A�dZA�M�A�%A�-A��uA�{A���A�XA��A�bA�A�A��`A�\)A�&�A��yA��7A�{A��\A�(�A��^A�^5A���A�=qA��HA�S�A��DA��7A��A�hsA�JA�{A��hA��A�  A�-A���A�=qA�|�A�ffA�dZA��A��A�~�A�33A�9XA���A��DA�oA�%A�A�XA�l�A���A���A�hsA��A��A�p�A�7LA���A���A�M�A���A��7A�9XA���A���A�%A��9A�bNA�p�A�t�A���A�jA�1'A�\)A��A��AC�A~�RA|��Ax��Aw�hAvffAu�Ar�/AqhsApjAn��AlI�Ai;dAf9XAe33Ad��Ac+A^��A\ �AZ��AXQ�AV=qARffAQ%AN�AL��AK�AL�AK�AK+AKK�AK7LAJ�9AH��AE�FAD�/AC�
ABffAA��A?��A?
=A>�HA>�HA>��A=oA:�`A9"�A7�A7;dA6r�A4ffA2�A1�A0�9A0ZA/�^A/
=A-+A+/A)�7A'O�A&�9A%�A%�A%
=A$��A$�jA$bA"~�A!ƨA!XA �9A�hAJAXAI�A��A=qAI�AȴA1'A�A�mA�A��A�AjAE�AK�A9XA
ĜA	��A��AQ�A�A��A;dA��A��A�`A|�A ��A  �@�1'@��w@�33@��@��#@�%@�(�@�V@�hs@�@�^5@�@���@� �@�R@�b@���@��y@���@� �@��y@�Ĝ@߶F@ݺ^@�G�@ܴ9@�t�@�-@١�@�O�@�?}@��@أ�@�
=@��@ԛ�@�b@�dZ@�~�@�{@�&�@��/@Л�@�A�@ϕ�@θR@̴9@�I�@� �@�t�@�~�@ɺ^@ȓu@�ƨ@ư!@���@�?}@�z�@ÍP@�=q@���@�O�@�9X@��m@��P@�
=@���@��T@�V@�  @���@�^5@�%@�1@�l�@��R@��T@�&�@��@��;@�l�@�@�@��@��D@�o@��!@�ff@�M�@��@��T@��-@�`B@��u@�I�@�b@��F@�|�@�t�@�t�@���@��R@�E�@��@�?}@��7@���@��@���@� �@��m@��;@���@�ƨ@��@�C�@���@�E�@�-@��T@�7L@���@�j@�Z@�(�@���@��w@���@�S�@�
=@��@��R@��\@�^5@�5?@��@���@���@�I�@��@��@��m@��F@���@�C�@�o@�
=@�@���@��@�@�x�@���@��u@���@��u@�z�@�j@�A�@�  @��@��;@��
@��P@�|�@�;d@���@�~�@�~�@��+@�v�@�v�@�n�@�ff@�V@�M�@�=q@�@��h@�p�@�/@��/@�Ĝ@��@��@�b@���@�b@�t�@�;d@���@��R@���@�~�@�5?@�@��#@��-@���@��h@�x�@�`B@�X@�O�@�7L@��/@��@��w@�C�@��y@���@�~�@�5?@�@��-@���@�`B@��D@��m@��P@�dZ@�o@�ȴ@�~�@�n�@��@��#@��@�O�@�&�@�V@���@��`@���@���@��D@�A�@�1@�ƨ@�\)@��@�o@�@�~�@�$�@��@�@��-@��-@���@��@�/@��@���@��u@�b@��@���@�dZ@�K�@�"�@�@���@��H@�ȴ@���@�ff@�E�@�5?@�-@�@��@�p�@�Ĝ@�bN@�9X@��@��F@���@�dZ@�@��y@��!@��\@�n�@�=q@�-@��@�x�@�/@��@���@���@�A�@��@+@~V@~{@}�h@|j@{�F@{��@{33@{dZ@{�@{�@{S�@{@z�@y�^@y&�@x�`@xb@w��@wl�@w
=@w�@w+@v�@v��@w;d@xb@w��@w��@w+@v�@v��@v$�@up�@u/@t��@t��@tz�@s�F@sdZ@s@r~�@r-@q��@q��@qx�@q&�@p�9@p�9@p�9@p1'@o�w@o�w@oK�@n5?@m��@mp�@m/@m�@l��@l�j@lZ@l1@kƨ@kdZ@j�@j��@i��@i�7@ihs@i%@h�u@hb@g
=@fȴ@fff@e�@e`B@d��@d�@d�/@d�@dz�@d1@c��@c@b�@a7L@a&�@`��@`�9@`Q�@_�;@_K�@^��@^ȴ@^�R@^��@^��@^v�@^V@^E�@^$�@]�T@]�@]�@\��@\��@\�@[�
@[dZ@[o@Z��@ZJ@Y�^@Y��@Y7L@X��@X �@W��@W��@Wl�@W
=@V�+@V5?@V$�@V{@U@Up�@U�@T��@T��@TI�@S��@S"�@S@R�H@R��@RJ@Q�7@Q&�@P�9@PbN@P �@O�@O�@O\)@Nȴ@N5?@N{@N{@N@M��@M?}@L�@Lz�@LI�@Kƨ@K��@KdZ@KC�@K@J��@J�\@Jn�@Jn�@JM�@I�^@I�7@IX@I7L@I%@H��@H�@HbN@HA�@H  @G��@G\)@G�@F��@Fff@FE�@F$�@F$�@E�@E��@Ep�@E�@D�/@D�@D��@D��@D�D@DI�@D1@C�F@Ct�@CC�@Co@B�H@B��@B�!@B~�@BM�@A�#@A�7@AX@A%@@Ĝ@@r�@@1'@@  @@  @?��@?�@>�y@>��@>�+@>v�@>ff@>V@>5?@>@=�T@=�T@=@=�@=O�@=/@<��@<�j@<��@<I�@;�m@;��@;dZ@:��@:=q@9��@9�7@97L@9&�@8�`@8bN@8  @7��@7l�@6�@6�+@6V@5�T@5O�@5/@4�/@4z�@4I�@3��@3t�@3o@2��@2=q@1�#@1�7@1G�@17L@0�`@0Ĝ@0��@0 �@/�w@/�P@/l�@.�y@.V@-��@-p�@-`B@-?}@,�@,��@,9X@+�
@+��@+��@+t�@*�@*��@*~�@*n�@*M�@)��@)�^@)�^@)�7@)hs@)�@(�9@(�@( �@'�w@'|�@'+@&�@&�R@&��@&v�@&{@%��@%`B@%�@$�/@$�@$z�@$I�@$�@#ƨ@#S�@#33@#"�@#@"�@"��@"n�@!��@!�^@!�7@!G�@!7L@!�@ ��@ Ĝ@ �9@ �@ Q�@ 1'@   @�;@�w@��@\)@��@��@ff@�@@�h@p�@`B@?}@/@/@�@��@�j@j@(�@��@ƨ@�@"�@�@��@n�@��@��@��@G�@&�@�`@Ĝ@�@1'@  @  @�@��@�w@�@l�@K�@;d@+@��@�R@�+@5?@�@�@�T@�T@��@�-@�h@�@p�@`B@?}@��@�/@�@�D@z�@j@9X@(�@�m@�F@�@S�@"�@��@~�@n�@M�@=q@-@�@��@��@��@�7@G�@&�@%@��@�@bN@ �@  @�;@�P@K�@;d@;d@
=@��@ȴ@�R@�R@�R@��@V@$�@@�T@��@�h@`B@�@�/@��@��@�j@��@Z@�@�m@�
@ƨ@dZ@"�@
�@
��@
��@
n�@
M�@
=q@
�@	�#@	�7@	X@	7L@	�@	%@�`@�9@�@bN@Q�@ �@  @�;@�w@�w111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Aݡ�Aݡ�Aݟ�Aݣ�Aݟ�Aݝ�Aݝ�Aݙ�Aݛ�Aݝ�Aݟ�Aݟ�Aݟ�Aݡ�Aݡ�Aݛ�A�|�A�ffA�$�AܼjA�$�A۲-A�oA�$�A��A�v�AӅA�M�A�M�A��A̧�A���A��HA�E�A�1'A�|�Aƺ^A��Aİ!A���A�v�A�dZA�M�A�%A�-A��uA�{A���A�XA��A�bA�A�A��`A�\)A�&�A��yA��7A�{A��\A�(�A��^A�^5A���A�=qA��HA�S�A��DA��7A��A�hsA�JA�{A��hA��A�  A�-A���A�=qA�|�A�ffA�dZA��A��A�~�A�33A�9XA���A��DA�oA�%A�A�XA�l�A���A���A�hsA��A��A�p�A�7LA���A���A�M�A���A��7A�9XA���A���A�%A��9A�bNA�p�A�t�A���A�jA�1'A�\)A��A��AC�A~�RA|��Ax��Aw�hAvffAu�Ar�/AqhsApjAn��AlI�Ai;dAf9XAe33Ad��Ac+A^��A\ �AZ��AXQ�AV=qARffAQ%AN�AL��AK�AL�AK�AK+AKK�AK7LAJ�9AH��AE�FAD�/AC�
ABffAA��A?��A?
=A>�HA>�HA>��A=oA:�`A9"�A7�A7;dA6r�A4ffA2�A1�A0�9A0ZA/�^A/
=A-+A+/A)�7A'O�A&�9A%�A%�A%
=A$��A$�jA$bA"~�A!ƨA!XA �9A�hAJAXAI�A��A=qAI�AȴA1'A�A�mA�A��A�AjAE�AK�A9XA
ĜA	��A��AQ�A�A��A;dA��A��A�`A|�A ��A  �@�1'@��w@�33@��@��#@�%@�(�@�V@�hs@�@�^5@�@���@� �@�R@�b@���@��y@���@� �@��y@�Ĝ@߶F@ݺ^@�G�@ܴ9@�t�@�-@١�@�O�@�?}@��@أ�@�
=@��@ԛ�@�b@�dZ@�~�@�{@�&�@��/@Л�@�A�@ϕ�@θR@̴9@�I�@� �@�t�@�~�@ɺ^@ȓu@�ƨ@ư!@���@�?}@�z�@ÍP@�=q@���@�O�@�9X@��m@��P@�
=@���@��T@�V@�  @���@�^5@�%@�1@�l�@��R@��T@�&�@��@��;@�l�@�@�@��@��D@�o@��!@�ff@�M�@��@��T@��-@�`B@��u@�I�@�b@��F@�|�@�t�@�t�@���@��R@�E�@��@�?}@��7@���@��@���@� �@��m@��;@���@�ƨ@��@�C�@���@�E�@�-@��T@�7L@���@�j@�Z@�(�@���@��w@���@�S�@�
=@��@��R@��\@�^5@�5?@��@���@���@�I�@��@��@��m@��F@���@�C�@�o@�
=@�@���@��@�@�x�@���@��u@���@��u@�z�@�j@�A�@�  @��@��;@��
@��P@�|�@�;d@���@�~�@�~�@��+@�v�@�v�@�n�@�ff@�V@�M�@�=q@�@��h@�p�@�/@��/@�Ĝ@��@��@�b@���@�b@�t�@�;d@���@��R@���@�~�@�5?@�@��#@��-@���@��h@�x�@�`B@�X@�O�@�7L@��/@��@��w@�C�@��y@���@�~�@�5?@�@��-@���@�`B@��D@��m@��P@�dZ@�o@�ȴ@�~�@�n�@��@��#@��@�O�@�&�@�V@���@��`@���@���@��D@�A�@�1@�ƨ@�\)@��@�o@�@�~�@�$�@��@�@��-@��-@���@��@�/@��@���@��u@�b@��@���@�dZ@�K�@�"�@�@���@��H@�ȴ@���@�ff@�E�@�5?@�-@�@��@�p�@�Ĝ@�bN@�9X@��@��F@���@�dZ@�@��y@��!@��\@�n�@�=q@�-@��@�x�@�/@��@���@���@�A�@��@+@~V@~{@}�h@|j@{�F@{��@{33@{dZ@{�@{�@{S�@{@z�@y�^@y&�@x�`@xb@w��@wl�@w
=@w�@w+@v�@v��@w;d@xb@w��@w��@w+@v�@v��@v$�@up�@u/@t��@t��@tz�@s�F@sdZ@s@r~�@r-@q��@q��@qx�@q&�@p�9@p�9@p�9@p1'@o�w@o�w@oK�@n5?@m��@mp�@m/@m�@l��@l�j@lZ@l1@kƨ@kdZ@j�@j��@i��@i�7@ihs@i%@h�u@hb@g
=@fȴ@fff@e�@e`B@d��@d�@d�/@d�@dz�@d1@c��@c@b�@a7L@a&�@`��@`�9@`Q�@_�;@_K�@^��@^ȴ@^�R@^��@^��@^v�@^V@^E�@^$�@]�T@]�@]�@\��@\��@\�@[�
@[dZ@[o@Z��@ZJ@Y�^@Y��@Y7L@X��@X �@W��@W��@Wl�@W
=@V�+@V5?@V$�@V{@U@Up�@U�@T��@T��@TI�@S��@S"�@S@R�H@R��@RJ@Q�7@Q&�@P�9@PbN@P �@O�@O�@O\)@Nȴ@N5?@N{@N{@N@M��@M?}@L�@Lz�@LI�@Kƨ@K��@KdZ@KC�@K@J��@J�\@Jn�@Jn�@JM�@I�^@I�7@IX@I7L@I%@H��@H�@HbN@HA�@H  @G��@G\)@G�@F��@Fff@FE�@F$�@F$�@E�@E��@Ep�@E�@D�/@D�@D��@D��@D�D@DI�@D1@C�F@Ct�@CC�@Co@B�H@B��@B�!@B~�@BM�@A�#@A�7@AX@A%@@Ĝ@@r�@@1'@@  @@  @?��@?�@>�y@>��@>�+@>v�@>ff@>V@>5?@>@=�T@=�T@=@=�@=O�@=/@<��@<�j@<��@<I�@;�m@;��@;dZ@:��@:=q@9��@9�7@97L@9&�@8�`@8bN@8  @7��@7l�@6�@6�+@6V@5�T@5O�@5/@4�/@4z�@4I�@3��@3t�@3o@2��@2=q@1�#@1�7@1G�@17L@0�`@0Ĝ@0��@0 �@/�w@/�P@/l�@.�y@.V@-��@-p�@-`B@-?}@,�@,��@,9X@+�
@+��@+��@+t�@*�@*��@*~�@*n�@*M�@)��@)�^@)�^@)�7@)hs@)�@(�9@(�@( �@'�w@'|�@'+@&�@&�R@&��@&v�@&{@%��@%`B@%�@$�/@$�@$z�@$I�@$�@#ƨ@#S�@#33@#"�@#@"�@"��@"n�@!��@!�^@!�7@!G�@!7L@!�@ ��@ Ĝ@ �9@ �@ Q�@ 1'@   @�;@�w@��@\)@��@��@ff@�@@�h@p�@`B@?}@/@/@�@��@�j@j@(�@��@ƨ@�@"�@�@��@n�@��@��@��@G�@&�@�`@Ĝ@�@1'@  @  @�@��@�w@�@l�@K�@;d@+@��@�R@�+@5?@�@�@�T@�T@��@�-@�h@�@p�@`B@?}@��@�/@�@�D@z�@j@9X@(�@�m@�F@�@S�@"�@��@~�@n�@M�@=q@-@�@��@��@��@�7@G�@&�@%@��@�@bN@ �@  @�;@�P@K�@;d@;d@
=@��@ȴ@�R@�R@�R@��@V@$�@@�T@��@�h@`B@�@�/@��@��@�j@��@Z@�@�m@�
@ƨ@dZ@"�@
�@
��@
��@
n�@
M�@
=q@
�@	�#@	�7@	X@	7L@	�@	%@�`@�9@�@bN@Q�@ �@  @�;@�w@�w111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�mB	�mB	�B	�B	�HB	ǮB	�B
�B
�B
%�B
L�B
�dB
�B>wBs�B�B��B��B��B�`B�B�B"�B�B�BJB�B!�B!�B+B!�BVB0!B8RBO�B[#BYBVBS�BS�BW
BXBXBS�BW
B]/BW
BVBM�BS�BM�B7LB&�BE�BK�B;dB7LB#�B��B�B��B�B�5B�9B��B�B��B�B��B��B�B��B�Bu�Bx�B�7B�%Bw�B`BB?}B9XB33B�B
��B
�B
�fB
��B
�mB
�
B
�FB
ȴB
ŢB
�9B
��B
��B
��B
�B
x�B
S�B
H�B
bNB
T�B
8RB
+B
�B
{B
	7B	�B	�B	�ZB	��B	�3B	��B	�\B	��B	��B	y�B	?}B	F�B	J�B	-B	 �B��B	
=B��B��B	B	JB	hB	�B	&�B	�B	bB��B�#B�sB�`B��B�B��B�B�TB�HB�B��B�B��B�!B�!B��B�\B�DB�=B�PB�\B�1B~�BiyBaHBaHB^5Bs�Bv�B�B�B�%B� Bt�BgmBq�Bs�BcTB33BD�B9XBK�BM�BA�B2-B@�BQ�BXBP�BC�B@�BH�BI�BN�BB�B5?B33B2-B<jB8RB/B49B@�BB�B<jB?}B8RBH�BI�B?}BbNBbNBbNB]/B]/B]/BYBaHB^5BbNBm�BiyBhsBe`BXBYB[#BdZBe`BffBgmBo�Bl�Bv�By�Br�Bu�B}�B� B�B~�Bw�Bo�Bp�B~�B�B}�B~�B�B�%B�bB�hB�VB�7B�%B�B�bB�oB�VB�DB�bB�\B��B��B��B��B��B��B��B�9B�RB�?B�wB��B�}B��B�}B��B�}BŢB��BBɺB��B��B�B�5B�`B�B��B��B��B��B	  B	B	hB	�B	�B	�B	�B	�B	�B	�B	'�B	/B	0!B	33B	7LB	8RB	6FB	8RB	9XB	9XB	B�B	H�B	G�B	D�B	G�B	I�B	M�B	S�B	VB	XB	ZB	XB	W
B	\)B	aHB	bNB	`BB	dZB	iyB	k�B	jB	k�B	m�B	o�B	r�B	t�B	x�B	y�B	z�B	z�B	{�B	{�B	|�B	z�B	�+B	�=B	�JB	�PB	�PB	�bB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�3B	�?B	�FB	�RB	�XB	�XB	�XB	�XB	�XB	�XB	�RB	�LB	�dB	�dB	�^B	�wB	�}B	�wB	�jB	�wB	�wB	�dB	�qB	��B	��B	ÖB	ŢB	ŢB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	�B	�B	�B	�B	�#B	�#B	�)B	�)B	�;B	�HB	�NB	�NB	�NB	�HB	�HB	�HB	�BB	�BB	�BB	�;B	�BB	�TB	�NB	�5B	�BB	�NB	�TB	�`B	�fB	�fB	�`B	�`B	�mB	�mB	�`B	�ZB	�sB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
%B
1B
1B
1B
+B
%B
	7B
	7B

=B
DB
PB
\B
\B
hB
oB
oB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
 �B
!�B
"�B
"�B
"�B
!�B
"�B
"�B
"�B
"�B
"�B
!�B
"�B
$�B
#�B
"�B
!�B
 �B
%�B
&�B
%�B
&�B
(�B
+B
+B
)�B
)�B
(�B
(�B
'�B
'�B
'�B
.B
-B
-B
,B
,B
,B
.B
/B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
/B
0!B
0!B
1'B
/B
1'B
1'B
1'B
1'B
1'B
2-B
49B
33B
2-B
33B
49B
5?B
5?B
49B
49B
6FB
7LB
7LB
6FB
6FB
7LB
7LB
7LB
7LB
6FB
7LB
9XB
9XB
8RB
6FB
8RB
8RB
9XB
:^B
:^B
:^B
:^B
:^B
9XB
:^B
<jB
=qB
<jB
;dB
;dB
<jB
;dB
=qB
<jB
>wB
>wB
?}B
>wB
?}B
?}B
?}B
@�B
?}B
>wB
?}B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
@�B
A�B
A�B
A�B
C�B
C�B
D�B
D�B
C�B
C�B
C�B
C�B
D�B
E�B
F�B
F�B
E�B
D�B
E�B
D�B
E�B
F�B
F�B
F�B
G�B
G�B
F�B
F�B
E�B
F�B
G�B
G�B
G�B
H�B
H�B
I�B
I�B
H�B
H�B
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
K�B
K�B
K�B
L�B
K�B
K�B
L�B
K�B
J�B
K�B
K�B
I�B
K�B
L�B
M�B
N�B
N�B
M�B
M�B
N�B
N�B
O�B
N�B
O�B
P�B
O�B
O�B
R�B
Q�B
Q�B
R�B
R�B
Q�B
R�B
R�B
S�B
T�B
VB
VB
W
B
VB
W
B
W
B
T�B
VB
W
B
W
B
T�B
VB
W
B
ZB
ZB
ZB
YB
YB
YB
YB
[#B
[#B
ZB
YB
[#B
\)B
\)B
\)B
\)B
\)B
]/B
\)B
]/B
\)B
\)B
]/B
]/B
]/B
]/B
^5B
^5B
_;B
_;B
_;B
^5B
^5B
^5B
`BB
`BB
`BB
`BB
aHB
aHB
`BB
`BB
cTB
cTB
cTB
cTB
bNB
bNB
bNB
cTB
dZB
dZB
e`B
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
e`B
e`B
dZB
e`B
ffB
e`B
gmB
gmB
hsB
hsB
hsB
hsB
iyB
hsB
hsB
gmB
gmB
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
k�B
k�B
k�B
k�B
k�B
l�B
m�B
m�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
l�B
l�B
l�B
l�B
m�B
o�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
n�B
n�B
n�B
n�B
o�B
o�B
p�B
p�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
o�B
p�B
r�B
q�B
r�B
r�B
r�B
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
r�B
s�B
t�B
s�B
t�B
u�B
u�B
u�B
u�B
t�B
u�B
v�B
u�B
u�B
t�B
u�B
u�B
u�B
v�B
u�B
u�B
u�B
v�B
w�B
w�B
w�B
v�B
v�B
w�B
w�B
x�B
x�B
w�B
w�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
x�B
x�B
y�B
z�B
z�B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
{�B
|�B
|�B
}�B
}�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	�B	�B	�B	��B	�B	�B	��B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	�KB	�B	��B	��B	�}B	�tB	�<B	��B
�B
�B
*�B
RB
�(B
�B@iBt�B�aB�jB��B��B�B�tB1B#B!B�B"B�B"�B#B,"B#�BoB3hB:�BQ B[�BY�BV�BUBUBXBYBY1BU�BX+B^BX�BW�BO�BU�BO�B:�B*BF�BMB>(B9�B'�B��B�zB�B�wB��B��B��B��B��B��B��B��B�!B�QB�tBw�By�B��B�_By�Bc BBuB<B4�B�B iB
�5B
��B
��B
�*B
ٴB
�*B
ɆB
��B
��B
�>B
�sB
�HB
�?B
{�B
X�B
K�B
b�B
V9B
;B
B
 B
9B
DB	�[B	�CB	��B	�<B	��B	�]B	��B	��B	��B	|�B	EB	I�B	L�B	0oB	#�B��B	JB	�B�B	�B	~B	TB	�B	&�B	 BB	�B��BޞB�B��B��B�KB��B�$B�B�B��B��B�B�B��B�'B�mB� B�PB��B�pB�B�RB��Bl=BdBc�B`�Bt�Bw�B��B��B��B��BvBiyBr�Bt�Bd�B7�BGB<�BM6BO(BC�B4�BB[BR�BX_BQ�BE9BBBI�BJ�BO�BDB72B5?B3�B=�B9�B1'B5�BA�BC�B>B@�B:*BI�BKDBBABb�Bb�Bb�B^B^B^BZQBa�B_�Bc:Bn/BjBi_Bf�BZBZ�B]BeFBf�Bg�Bh�Bp�Bm�BwLBz�Bs�Bv�B~wB�OB�UBcBx�Bp�Bq�BcB��B~�B�B��B��B��B��B��B��B��B�uB��B��B�B�0B�B�bB�?B��B�pB�tB��B��B��B��B��B�+B��B��B� B�B�4B�AB�iB�?B��BÖBʌBӏBԯB��B��B��B�5B�LB�jB��B��B	 �B	%B	�B	�B	�B	�B	�B	B	B	pB	(>B	/OB	0oB	3�B	7�B	8�B	6�B	8�B	9�B	:B	B�B	H�B	G�B	EB	HB	J#B	N"B	TB	V9B	XEB	ZkB	X�B	W�B	\�B	a|B	b�B	`�B	d�B	i�B	k�B	j�B	k�B	m�B	o�B	r�B	uB	y$B	y�B	{0B	{0B	|6B	|PB	}VB	{�B	�zB	��B	�~B	�jB	��B	�}B	��B	��B	��B	��B	�B	�B	��B	��B	�1B	��B	��B	�B	�B	�B	�&B	�&B	�B	�$B	�0B	�KB	�CB	�kB	��B	�MB	�ZB	�`B	��B	�rB	��B	�rB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ðB	��B	��B	��B	��B	��B	�B	�B	��B	��B	�B	��B	�B	�7B	�RB	�6B	�BB	�TB	�FB	�2B	�FB	�MB	�FB	�9B	�aB	ѝB	�NB	�gB	�KB	�yB	�_B	�kB	�=B	یB	�xB	�xB	�pB	�|B	�B	�B	�B	�|B	�B	�|B	��B	��B	��B	ߤB	��B	�B	�B	޸B	��B	�B	�B	�B	�fB	�B	�B	�B	�B	�B	��B	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�/B	��B	��B	�B	�B	��B	�B	�2B	�B	�0B	�B	�B	�BB
 4B	�BB	�dB	�<B	�VB	�(B	�BB	�VB	�6B	�BB	�PB	�.B	�VB	�dB	�(B
-B
GB
?B
KB
fB
fB
zB
�B
	�B
	�B

�B
�B
�B
�B
�B
�B
�B
�B
�B
sB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
B
B
�B
B
)B
�B
 �B
!�B
#B
#B
#B
"B
#B
#B
# B
# B
"�B
"4B
# B
$�B
$&B
# B
"B
!HB
&B
'8B
&2B
'8B
)*B
+B
+6B
*0B
*0B
)DB
)DB
(XB
(XB
(XB
./B
-)B
-CB
,WB
,WB
,=B
./B
/OB
0;B
0;B
0;B
0UB
0UB
0UB
0UB
0oB
/iB
0UB
0UB
1[B
/iB
1vB
1vB
1[B
1vB
1vB
2aB
4nB
3�B
2|B
3hB
4nB
5tB
5tB
4�B
4�B
6zB
7fB
7�B
6zB
6zB
7�B
7�B
7�B
7�B
6�B
7�B
9�B
9rB
8�B
6�B
8�B
8�B
9�B
:�B
:�B
:�B
:�B
:�B
9�B
:�B
<�B
=�B
<�B
;�B
;�B
<�B
;�B
=�B
<�B
>�B
>�B
?�B
>�B
?�B
?�B
?�B
@�B
?�B
>�B
?�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
@�B
A�B
A�B
A�B
C�B
C�B
D�B
D�B
C�B
C�B
C�B
C�B
D�B
E�B
F�B
F�B
E�B
D�B
E�B
D�B
E�B
F�B
F�B
F�B
G�B
G�B
F�B
F�B
E�B
F�B
G�B
G�B
G�B
H�B
H�B
I�B
I�B
IB
H�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
MB
L�B
K�B
K�B
K�B
MB
K�B
K�B
MB
K�B
KB
K�B
K�B
J=B
LB
MB
NB
OB
OB
NB
N"B
OB
O(B
PB
O(B
PB
QB
P.B
P.B
SB
R B
R:B
S&B
S&B
R:B
S&B
S@B
T,B
UB
VB
V9B
W$B
V9B
W$B
W?B
UMB
V9B
W?B
W?B
UMB
VmB
WYB
Z7B
ZQB
Z7B
YKB
Y1B
YKB
YeB
[WB
[WB
ZQB
YeB
[=B
\]B
\]B
\CB
\]B
\]B
]IB
\]B
]dB
\]B
\]B
]dB
]~B
]dB
]dB
^jB
^jB
_�B
_pB
_pB
^jB
^jB
^jB
`vB
`vB
`\B
`\B
a|B
a|B
`\B
`�B
cnB
cnB
c�B
cnB
b�B
b�B
b�B
c�B
d�B
d�B
e`B
d�B
dtB
ezB
e�B
e�B
e�B
e�B
ezB
ezB
e�B
e�B
e�B
d�B
e�B
f�B
e�B
g�B
g�B
h�B
h�B
h�B
h�B
i�B
h�B
h�B
g�B
g�B
h�B
h�B
h�B
h�B
h�B
i�B
i�B
i�B
i�B
j�B
j�B
j�B
k�B
k�B
k�B
k�B
k�B
l�B
m�B
m�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
l�B
l�B
l�B
l�B
m�B
o�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
n�B
n�B
n�B
n�B
o�B
o�B
p�B
p�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
o�B
p�B
r�B
q�B
r�B
r�B
r�B
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
r�B
s�B
t�B
s�B
t�B
u�B
u�B
u�B
u�B
t�B
u�B
v�B
u�B
u�B
uB
u�B
u�B
u�B
v�B
u�B
u�B
u�B
v�B
w�B
w�B
w�B
v�B
v�B
xB
xB
x�B
y	B
xB
xB
y	B
y	B
y	B
zB
zB
y�B
zB
y	B
x�B
zB
{B
{B
z�B
{B
z�B
{B
|B
|B
|B
|B
}B
}"B
~B
~111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.19(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201808070037382018080700373820180807003738201808070200252018080702002520180807020025201808080023592018080800235920180808002359  JA  ARFMdecpA19c                                                                20180803093510  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180803003525  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180803003528  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180803003529  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180803003529  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180803003529  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180803003529  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180803003529  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180803003530  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180803003530                      G�O�G�O�G�O�                JA  ARUP                                                                        20180803005723                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180803153359  CV  JULD            G�O�G�O�Fõ�                JM  ARGQJMQC2.0                                                                 20180803153359  CV  JULD_LOCATION   G�O�G�O�Fõ�                JM  ARGQJMQC2.0                                                                 20180803153359  CV  LATITUDE        G�O�G�O�A�A�                JM  ARGQJMQC2.0                                                                 20180803153359  CV  LONGITUDE       G�O�G�O��"�q                JM  ARCAJMQC2.0                                                                 20180806153738  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180806153738  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180806170025  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180807152359  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116231517                      G�O�G�O�G�O�                