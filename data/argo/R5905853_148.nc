CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2023-02-28T03:43:34Z creation;2023-02-28T03:43:35Z conversion to V3.1      
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
_FillValue                 �  I4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p|   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tl   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �X   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �t   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ޤ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �0   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �4   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �D   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �H   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �L   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20230228034334  20230228040047  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @�<f5��1   @�<�#E@/U�$�/�c)�-V1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @�  @�  A   A   AA��A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  BhffBp  Bx  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B���B�  B�  B�  B�  Bԙ�B�33B���B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C�C�C  C  C�C�fC�fC"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<33C>  C@  CB  CD  CF  CH  CJ  CL  CN  CO�fCR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#�fD$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڃ3D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�ff111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @s�
@��@��A��A>�]A\��A|��A�z�A�z�A�z�A�z�A�z�A�z�A�z�A�z�B=qB=qB=qB=qB'=qB/=qB7=qB?=qBG=qBO=qBW=qB_=qBg��Bo=qBw=qB=qB���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B�k�BÞ�BǞ�B˞�BϞ�B�8RB���B�k�Bߞ�B㞸B瞸B랸BB�B���B���B���C�\C�\C�\C�\C	�\C�\C�\C�\C�\C��C��C�\C�\C��C��C��C!�\C#�\C%�\C'�\C)�\C+�\C-�\C/�\C1�\C3�\C5�\C7�\C9�\C<�C=�\C?�\CA�\CC�\CE�\CG�\CI�\CK�\CM�\CO��CQ�\CS�\CU�\CW�\CY�\C[�\C]�\C_�\Ca�\Cc�\Ce�\Cg�\Ci�\Ck�\Cm�\Co�\Cq�\Cs�\Cu�\Cw�\Cy�\C{�\C}�\C�\C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��{C��{C��C��C��C��C��C��{C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��{C��C��C��C��C��C��C��C��C��C��C��C��{C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��D s�D ��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��D	s�D	��D
s�D
��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D�qDs�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��D s�D ��D!s�D!��D"s�D"��D#z=D#��D$s�D$��D%s�D%��D&s�D&��D's�D'��D(s�D(��D)s�D)��D*s�D*��D+s�D+��D,s�D,��D-s�D-��D.s�D.��D/s�D/��D0s�D0��D1s�D1��D2s�D2��D3s�D3��D4s�D4��D5s�D5��D6s�D6��D7s�D7��D8s�D8��D9s�D9��D:s�D:��D;s�D;��D<s�D<��D=s�D=��D>s�D>��D?s�D?��D@s�D@��DAs�DA��DBs�DB��DCs�DC��DDs�DD��DEs�DE��DFs�DF��DGs�DG��DHs�DH��DIs�DI��DJs�DJ��DKs�DK��DLs�DL��DMs�DM��DNs�DN��DOs�DO��DPs�DP��DQs�DQ��DRs�DR��DSs�DS��DTs�DT��DUs�DU��DVs�DV��DWs�DW��DXs�DX��DYs�DY��DZs�DZ��D[s�D[��D\s�D\��D]s�D]��D^s�D^��D_s�D_��D`s�D`��Das�Da��Dbs�Db��Dcs�Dc��Dds�Dd��Des�De��Dfs�Df��Dgs�Dg��Dhs�Dh��Dis�Di��Djs�Dj��Dks�Dk��Dls�Dl��Dms�Dm��Dns�Dn��Dos�Do��Dps�Dp��Dqs�Dq��Drs�Dr��Dss�Ds��Dts�Dt��Dus�Du��Dvs�Dv��Dws�Dw��Dxs�Dx��Dys�Dy��Dzs�Dz��D{s�D{��D|s�D|��D}s�D}��D~s�D~��Ds�D��D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D��D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D¹�D���D�9�D�y�Dù�D���D�9�D�y�DĹ�D���D�9�D�y�DŹ�D���D�9�D�y�Dƹ�D���D�9�D�y�Dǹ�D���D�9�D�y�Dȹ�D���D�9�D�y�Dɹ�D���D�9�D�y�Dʹ�D���D�9�D�y�D˹�D���D�9�D�y�D̹�D���D�9�D�y�D͹�D���D�9�D�y�Dι�D���D�9�D�y�DϹ�D���D�9�D�y�Dй�D���D�9�D�y�Dѹ�D���D�9�D�y�Dҹ�D���D�9�D�y�Dӹ�D���D�9�D�y�DԹ�D���D�9�D�y�Dչ�D���D�9�D�y�Dֹ�D���D�9�D�y�D׹�D���D�9�D�y�Dع�D���D�9�D�y�Dٹ�D���D�9�D�}Dڹ�D���D�9�D�y�D۹�D���D�9�D�y�Dܹ�D���D�9�D�y�Dݹ�D���D�9�D�y�D޹�D���D�9�D�y�D߹�D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D���D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D�D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�`R111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�A�A��TA���A���A�چA��A��A�ɺA��mA��mA��mA��9A��aA��[A���A���A��UA���A�A���Aĺ�Aĵ�AķLAĸRAĸAĸ�Aĸ�AĹ�Aĺ�AĻ0AļAĻ�Aĺ^AĹ�AļAĽqA���A��<A��#A��A��A�GzA�bNA�"�A�I�A��@A��A�0�A�9�A��A�{A�A�A��5A���A�K�A�VA��:A��A��A��4A���A��tA���A���A��A��kA�A���A���A��A���A���A���A�#A��A���A���A��A��A��A�S�A�x8A�tTA�AA��3A���A�f�A�1�A�U2A|_Ax�jAv-Ak:*Ae��AeYAc�Ab��Aa �A\xAZ3�AY��AXVmAVJAT�AQZ�AN�AK5?AI��AG1AF33AC�oA@~�A>	A<�9A<��A<[�A;�*A:�A:($A81'A6rGA4�A4m]A3�{A3=A2�kA1��A1��A1bNA1�A0�uA/��A.�A,��A*�A*�A(��A(>�A&�BA%4�A$zxA"��A!�;A!�4A!3�A �A *0As�A�A�SA:*A�EAoA��A�A:�AC�A�A\�ASA�mA�MA{�A��A�AaAU2A%APHA�oAs�A1�A#:A;A�rA��A�PA
�:A
��A
��A
|�A	YAZA�CA�PA#:A�A;A^5AiDAVmA��A��A*�A��A�A��A_pA]�A��Ae,A~An/A\�AFA �-A ��A 2�@���@�4�@�k�@���@��!@��1@�w�@�/�@�4@��@���@�%@�	@��@��@�Y@��
@�x@�Q�@��@�(�@���@��@@�:�@��g@�;@�?@�;d@�z@�5?@��D@��+@��@��@�$@���@��@�x@�|@�S@�j@�Ta@�=@Ⴊ@�q@��@߾w@�*0@�xl@���@�]�@ܾ�@��@ۮ@�1'@وf@�m]@�S�@��@��@�@�͟@�,=@��D@ׂ�@��@�L0@�*0@��X@�l�@�7@һ�@�n/@��X@�_@�7@Ϣ�@�%F@��@���@ͪ�@�m]@�ѷ@̚@�tT@�  @ˣn@�j�@�@ʯO@ʖ�@�O@��@Ɉf@��@ȧ�@��@�zx@�N<@��U@���@�zx@�Ĝ@��]@�O@��@ �@u@�9X@��[@���@�L�@�ں@���@�N�@��)@�p�@�@��@�^�@�%@���@���@��@�C-@�1@���@�P�@�4�@��E@�%�@���@�A�@�#�@��f@���@�*�@���@���@�}�@�8�@���@�^5@�@��;@���@�Z�@��@���@�1�@���@�dZ@���@�1'@���@���@��@��"@�s�@��@�ff@��D@�9�@��F@�n/@��@��N@�(�@�hs@��@��h@�s�@��@�8@��@��8@��z@�E�@���@��P@��@��)@���@�u�@�c�@�	�@��}@���@��{@�t�@�l�@�O@�X@�O@�Y@�҉@��@�r�@�g8@�GE@�8�@��&@�}�@���@�J@�n/@�,�@���@���@���@�d�@�@�o�@�Y�@�C�@�"�@�	l@��/@���@���@���@�J@�Q�@�	l@��@���@�u%@�4@��9@�A @���@�M�@��;@���@�Z�@�7L@��	@���@�:*@�@��@���@��0@�m]@�C@��@�͟@���@�q@��@��P@�/@��/@���@�z@�n�@�*�@���@�a@�`B@��|@��R@�PH@��@��@��7@�rG@�2a@���@��I@�xl@�e�@�_@�!@���@�o�@�P�@�>�@�%F@���@��z@��@��@���@��4@�~�@�v`@�#�@���@�Xy@�-@��@���@�H�@�Y@��@��9@���@�h
@�Z�@�A�@��]@��@�x@�:�@�;@���@�z�@�0U@�e@��r@��@���@�`B@�>�@�$t@��f@���@�L0@��V@�RT@�Y@���@��B@��U@���@�U2@�>B@�'R@��@��0@��@��'@�z�@�	@�ƨ@���@�B�@�(@�Ɇ@���@�p;@�@�*@~�R@~R�@~:*@~e@}��@|Ĝ@|S�@{�]@{�k@{s@{F�@{�@z�+@y��@y��@x�@x��@w�+@v��@v.�@u��@t֡@tS�@t>B@tD�@so@r�@q��@qDg@ph�@ot�@n��@nl�@m|@m-w@m;@l�p@ly>@k��@k_p@k,�@j��@j �@iϫ@i�h@i:�@h�`@hA�@gƨ@g��@gg�@g.I@f�x@e��@e�9@e��@eS&@e�@dZ@d�@c�@c�Q@c˒@c��@cA�@b��@b_�@bB[@be@a�z@a��@aF@`�@`[�@_��@_�[@_iD@_J#@_/�@_�@^�@^� @^u@]zx@]X@\�|@\?�@\"h@[��@[t�@[6z@Z�@Z�!@Zn�@Z3�@YT�@X��@X|�@Xj@X:�@W�m@Wv`@W(@V�+@U�t@UF@U+@T�@T��@Tb@S��@S�@S�@R�@R1�@Q��@Q/@Q(�@Q@@P�@PĜ@P��@PC-@P@O��@O�a@Ob�@N�c@N��@N��@N.�@M��@Mx�@L�5@L_@L~@K�P@K�@J�F@J&�@I�7@Im]@IV@H�	@H�`@H��@HC-@G�@Gv`@G�@F�h@F5?@E�@E�@EVm@E4@D�$@DS�@C�6@C�V@C$t@B��@BYK@B!�@A��@A�@A\�@@�@@�u@@Q�@?�r@?��@?��@?Mj@>͟@>�L@>a|@>@>{@=�)@=rG@=q@<�p@<�e@<�@<V�@;��@;\)@;�@:��@:E�@:e@9��@9e,@9�@8��@8h�@7�r@7��@7l�@7�@6�]@6�R@6��@6�@6@�@5�@5o @55�@4�@4tT@4bN@4<�@4<�@46@3��@3��@3y�@3j�@3F�@3o@2��@2p;@2R�@1�j@1j@0�@0�U@0�@0y>@0�@/�}@/�@/RT@.�@.҉@.��@.ff@.�@-��@-N<@,��@,7@+�a@+�k@+�$@+�P@+�f@+\)@+�@*�1@*q�@*L0@)�.@)��@)u�@(��@(��@(��@(�e@(��@(j@(M@'�;@'�w@'��@'~�@'8@&�@&��@&E�@&_@%ԕ@%�n@%�7@%f�@%�@$��@$��@$�$@$��@$I�@$�@$�@#��@#�V@#]�@#@O@#C@"�X@"��@"R�@"5?@"&�@!�@!�@!�M@!%F@ �@ �5@ �@ ��@ M@ %�@ G@�0@��@t�@g�@Mj@8@�@�@�R@~�@!�@�.@�D@�#@�h@zx@O�@-w@&�@�|@r�@K^@-�@��@�K@��@Mj@9�@�@�@��@҉@�!@��@a|@#:@�@��@c�@B�@%F@�@�/@��@��@oi@Q�@:�@x@��@��@��@n/@]�@J#@8@�@��@�@�<@��@��@� @q�@;�@)�@�@��@�-@�"@L�@@�`@Ĝ@�4@��@_@  @�@��@�$@O@�8@ߤ@ߤ@�h@� @^5@8�@@��@�z@��@��@u�@\�@8�@V@�|@�p@�@�.@w�@bN@K^@@�@�A@�*@y�@Z�@
=@��@��@W�@L0@E�@_@��@�>@��@��@q@%@�@֡@��@�@��@l"@Xy@:�@/�@1'@x@� @�@��@��@n/@/�@
�s@
�h@
��@
~�@
c @
M�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�A�A��TA���A���A�چA��A��A�ɺA��mA��mA��mA��9A��aA��[A���A���A��UA���A�A���Aĺ�Aĵ�AķLAĸRAĸAĸ�Aĸ�AĹ�Aĺ�AĻ0AļAĻ�Aĺ^AĹ�AļAĽqA���A��<A��#A��A��A�GzA�bNA�"�A�I�A��@A��A�0�A�9�A��A�{A�A�A��5A���A�K�A�VA��:A��A��A��4A���A��tA���A���A��A��kA�A���A���A��A���A���A���A�#A��A���A���A��A��A��A�S�A�x8A�tTA�AA��3A���A�f�A�1�A�U2A|_Ax�jAv-Ak:*Ae��AeYAc�Ab��Aa �A\xAZ3�AY��AXVmAVJAT�AQZ�AN�AK5?AI��AG1AF33AC�oA@~�A>	A<�9A<��A<[�A;�*A:�A:($A81'A6rGA4�A4m]A3�{A3=A2�kA1��A1��A1bNA1�A0�uA/��A.�A,��A*�A*�A(��A(>�A&�BA%4�A$zxA"��A!�;A!�4A!3�A �A *0As�A�A�SA:*A�EAoA��A�A:�AC�A�A\�ASA�mA�MA{�A��A�AaAU2A%APHA�oAs�A1�A#:A;A�rA��A�PA
�:A
��A
��A
|�A	YAZA�CA�PA#:A�A;A^5AiDAVmA��A��A*�A��A�A��A_pA]�A��Ae,A~An/A\�AFA �-A ��A 2�@���@�4�@�k�@���@��!@��1@�w�@�/�@�4@��@���@�%@�	@��@��@�Y@��
@�x@�Q�@��@�(�@���@��@@�:�@��g@�;@�?@�;d@�z@�5?@��D@��+@��@��@�$@���@��@�x@�|@�S@�j@�Ta@�=@Ⴊ@�q@��@߾w@�*0@�xl@���@�]�@ܾ�@��@ۮ@�1'@وf@�m]@�S�@��@��@�@�͟@�,=@��D@ׂ�@��@�L0@�*0@��X@�l�@�7@һ�@�n/@��X@�_@�7@Ϣ�@�%F@��@���@ͪ�@�m]@�ѷ@̚@�tT@�  @ˣn@�j�@�@ʯO@ʖ�@�O@��@Ɉf@��@ȧ�@��@�zx@�N<@��U@���@�zx@�Ĝ@��]@�O@��@ �@u@�9X@��[@���@�L�@�ں@���@�N�@��)@�p�@�@��@�^�@�%@���@���@��@�C-@�1@���@�P�@�4�@��E@�%�@���@�A�@�#�@��f@���@�*�@���@���@�}�@�8�@���@�^5@�@��;@���@�Z�@��@���@�1�@���@�dZ@���@�1'@���@���@��@��"@�s�@��@�ff@��D@�9�@��F@�n/@��@��N@�(�@�hs@��@��h@�s�@��@�8@��@��8@��z@�E�@���@��P@��@��)@���@�u�@�c�@�	�@��}@���@��{@�t�@�l�@�O@�X@�O@�Y@�҉@��@�r�@�g8@�GE@�8�@��&@�}�@���@�J@�n/@�,�@���@���@���@�d�@�@�o�@�Y�@�C�@�"�@�	l@��/@���@���@���@�J@�Q�@�	l@��@���@�u%@�4@��9@�A @���@�M�@��;@���@�Z�@�7L@��	@���@�:*@�@��@���@��0@�m]@�C@��@�͟@���@�q@��@��P@�/@��/@���@�z@�n�@�*�@���@�a@�`B@��|@��R@�PH@��@��@��7@�rG@�2a@���@��I@�xl@�e�@�_@�!@���@�o�@�P�@�>�@�%F@���@��z@��@��@���@��4@�~�@�v`@�#�@���@�Xy@�-@��@���@�H�@�Y@��@��9@���@�h
@�Z�@�A�@��]@��@�x@�:�@�;@���@�z�@�0U@�e@��r@��@���@�`B@�>�@�$t@��f@���@�L0@��V@�RT@�Y@���@��B@��U@���@�U2@�>B@�'R@��@��0@��@��'@�z�@�	@�ƨ@���@�B�@�(@�Ɇ@���@�p;@�@�*@~�R@~R�@~:*@~e@}��@|Ĝ@|S�@{�]@{�k@{s@{F�@{�@z�+@y��@y��@x�@x��@w�+@v��@v.�@u��@t֡@tS�@t>B@tD�@so@r�@q��@qDg@ph�@ot�@n��@nl�@m|@m-w@m;@l�p@ly>@k��@k_p@k,�@j��@j �@iϫ@i�h@i:�@h�`@hA�@gƨ@g��@gg�@g.I@f�x@e��@e�9@e��@eS&@e�@dZ@d�@c�@c�Q@c˒@c��@cA�@b��@b_�@bB[@be@a�z@a��@aF@`�@`[�@_��@_�[@_iD@_J#@_/�@_�@^�@^� @^u@]zx@]X@\�|@\?�@\"h@[��@[t�@[6z@Z�@Z�!@Zn�@Z3�@YT�@X��@X|�@Xj@X:�@W�m@Wv`@W(@V�+@U�t@UF@U+@T�@T��@Tb@S��@S�@S�@R�@R1�@Q��@Q/@Q(�@Q@@P�@PĜ@P��@PC-@P@O��@O�a@Ob�@N�c@N��@N��@N.�@M��@Mx�@L�5@L_@L~@K�P@K�@J�F@J&�@I�7@Im]@IV@H�	@H�`@H��@HC-@G�@Gv`@G�@F�h@F5?@E�@E�@EVm@E4@D�$@DS�@C�6@C�V@C$t@B��@BYK@B!�@A��@A�@A\�@@�@@�u@@Q�@?�r@?��@?��@?Mj@>͟@>�L@>a|@>@>{@=�)@=rG@=q@<�p@<�e@<�@<V�@;��@;\)@;�@:��@:E�@:e@9��@9e,@9�@8��@8h�@7�r@7��@7l�@7�@6�]@6�R@6��@6�@6@�@5�@5o @55�@4�@4tT@4bN@4<�@4<�@46@3��@3��@3y�@3j�@3F�@3o@2��@2p;@2R�@1�j@1j@0�@0�U@0�@0y>@0�@/�}@/�@/RT@.�@.҉@.��@.ff@.�@-��@-N<@,��@,7@+�a@+�k@+�$@+�P@+�f@+\)@+�@*�1@*q�@*L0@)�.@)��@)u�@(��@(��@(��@(�e@(��@(j@(M@'�;@'�w@'��@'~�@'8@&�@&��@&E�@&_@%ԕ@%�n@%�7@%f�@%�@$��@$��@$�$@$��@$I�@$�@$�@#��@#�V@#]�@#@O@#C@"�X@"��@"R�@"5?@"&�@!�@!�@!�M@!%F@ �@ �5@ �@ ��@ M@ %�@ G@�0@��@t�@g�@Mj@8@�@�@�R@~�@!�@�.@�D@�#@�h@zx@O�@-w@&�@�|@r�@K^@-�@��@�K@��@Mj@9�@�@�@��@҉@�!@��@a|@#:@�@��@c�@B�@%F@�@�/@��@��@oi@Q�@:�@x@��@��@��@n/@]�@J#@8@�@��@�@�<@��@��@� @q�@;�@)�@�@��@�-@�"@L�@@�`@Ĝ@�4@��@_@  @�@��@�$@O@�8@ߤ@ߤ@�h@� @^5@8�@@��@�z@��@��@u�@\�@8�@V@�|@�p@�@�.@w�@bN@K^@@�@�A@�*@y�@Z�@
=@��@��@W�@L0@E�@_@��@�>@��@��@q@%@�@֡@��@�@��@l"@Xy@:�@/�@1'@x@� @�@��@��@n/@/�@
�s@
�h@
��@
~�@
c @
M�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B	>BB	>B	=�B	=VB	=VB	=qB	=<B	=VB	=qB	=qB	=VB	=qB	=qB	<�B	=�B	=�B	=�B	=�B	>(B	>(B	>BB	>wB	>�B	>�B	>wB	>�B	>�B	>�B	>�B	>�B	>�B	?B	?HB	@ B	?�B	?�B	@�B	BAB	GB	L�B	R�B	q�B	�)B
:xB
N�B
��B
ݘB
�B
��BSB"hB$�B vB&LB;dBK^BY1B[�B]�B]dBX�BoiB~(B��B}VByrBl�Bg�BbNBI�B?�B<B:�B6B1ABN�BRTB88B
�CB
��B
vB
P�B
@4B
?�B
8�B
%FB	�jB	͟B	��B	��B	��B	x�B	kB	9	B	)B	yB	oB	
=B	 4B�qB�bB��BخBԕB� B�KB��B��B�|B�)B�B�ZB��B��B�^B��B�=B��B�YB��B��B�
B��B�(B�BB��B��B�pB��B��B�QB��B�GB��B�B��B�8B�9B�^BуB�_BܬB�nB�wB�B	UB	dB	(B	B	�B	!�B	 BB	$B	%�B	+�B	0!B	+�B	4TB	9�B	:�B	:�B	:�B	<PB	AoB	CaB	CGB	G_B	J�B	K^B	`�B	p�B	{JB	s�B	rB	|�B	�4B	��B	�GB	z�B	��B	��B	�B	�iB	|�B	xB	xB	v�B	xRB	z�B	��B	��B	�B	�B	�=B	��B	�EB	�lB	�^B	��B	��B	�NB	��B	��B	��B	�BB	��B	��B	�DB	��B	�HB	��B	�YB	��B	�B	��B	�sB	�B	�B	�1B	��B	��B	�gB	�B	�
B	��B	�mB	�YB	�yB	��B	�B	��B	��B	�]B	��B	�	B	��B	��B	�pB	�NB	��B	�fB	��B	��B	�B	�kB	�qB	�B	�B	��B	��B	��B	��B	��B	�rB	��B	�4B	�{B	ĶB	��B	��B	�B	ȀB	�+B	��B	�mB	�YB	�zB	�B	�KB	ȀB	��B	�=B	̘B	͹B	ΥB	�vB	�NB	� B	��B	҉B	�oB	�NB	�NB	��B	ѷB	��B	��B	�FB	ѷB	� B	��B	յB	�B	�B	׍B	چB	ۦB	��B	�xB	��B	�B	޸B	��B	߾B	ߤB	�HB	�B	�B	��B	��B	�B	�B	�-B	��B	�HB	��B	��B	�B	�B	�vB	��B	�B	��B	�B	��B	�B	�@B	�tB	�nB	��B	�2B	�`B	�FB	�B	�B	��B	��B	�B	�mB	�
B	�sB	�sB	�XB	�sB	��B	�eB	��B	�B	�B	��B	��B	�CB	��B	��B	�B	�B	��B	�B	��B	��B	�oB	�B	��B	�B	�?B	�?B	�ZB	��B	��B	��B	�LB	��B	�AB	�B	��B	��B	�`B	�?B	�%B	�%B	�ZB	��B	�FB	��B	�+B	�B	��B	�lB	�B	�	B	��B	�JB	��B	�B	�jB	��B	�"B	��B	�B	��B
�B
�B
+B
�B
�B
	RB
	�B
	lB

rB
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
JB
B
dB
~B
JB
dB
JB
dB
0B
0B
B
�B
�B
�B
VB
<B
<B
VB
VB
B
�B
�B
dB
JB
�B
�B
�B
dB
~B
�B
B
�B
BB
4B
�B
�B
&B
�B
2B
MB
�B
�B
SB
B
�B
�B
9B
B
eB
)B
B
�B
�B
�B
�B
�B
5B
5B
B
!B
 B
 vB
 vB
!HB
!�B
!�B
"4B
"hB
"�B
#nB
#�B
$&B
#�B
$@B
$ZB
$@B
$&B
%,B
&B
&LB
&fB
&fB
'8B
'mB
'mB
(
B
'�B
'�B
($B
(>B
(
B
($B
(�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
*eB
*�B
+B
+�B
+�B
+�B
+�B
+�B
,�B
-CB
-�B
-wB
-�B
-�B
./B
.�B
/5B
/iB
/�B
/�B
/�B
0!B
0�B
1�B
2GB
2�B
3B
3�B
3�B
4B
4B
49B
4�B
5B
5�B
5�B
5�B
5�B
5�B
6+B
5�B
6+B
6FB
6+B
5�B
5�B
5�B
5�B
5�B
5�B
5tB
5ZB
4�B
4�B
4�B
5�B
6B
6`B
6�B
8lB
7�B
88B
7�B
8lB
9	B
9XB
9rB
9�B
9XB
9�B
9�B
:xB
:DB
:�B
;dB
;0B
;dB
;�B
<B
<6B
<6B
<6B
<PB
<�B
<�B
<�B
=<B
>]B
>�B
>�B
?}B
@�B
A B
A B
A�B
A�B
A�B
A�B
B�B
C-B
C�B
C�B
D3B
DgB
D�B
EB
ESB
E�B
F?B
F�B
F�B
F�B
F�B
F�B
F�B
GB
G_B
GzB
G�B
GzB
F�B
G_B
GEB
G�B
G�B
G�B
HB
G�B
HB
IB
I�B
I�B
I�B
I�B
I�B
I�B
J#B
J�B
K^B
K�B
K�B
L0B
MB
MPB
MPB
M�B
MPB
MjB
N�B
OB
OvB
OBB
OvB
O�B
O\B
O�B
PB
PbB
P}B
P�B
QB
QhB
QhB
Q�B
RoB
R�B
R�B
R�B
SB
TFB
TaB
T�B
T{B
T�B
UgB
UMB
U�B
UgB
UgB
U�B
U�B
VB
VSB
V�B
V�B
WYB
W�B
W�B
XB
W�B
X�B
X�B
Y�B
YeB
ZB
Z�B
Z�B
Z�B
Z�B
[	B
[	B
[	B
Z�B
[	B
[qB
[WB
[	B
[�B
[�B
[�B
[�B
\]B
\�B
\�B
]B
]dB
]dB
]IB
]/B
]B
]�B
^B
^5B
^�B
_B
_B
_VB
`'B
`�B
`�B
aB
a�B
a�B
a�B
bB
bNB
bhB
b�B
b�B
c�B
d&B
c�B
d�B
d�B
eB
e,B
e`B
eFB
ezB
fLB
fLB
f2B
f2B
f�B
f�B
f�B
f�B
f�B
f�B
gRB
g�B
h
B
h
B
g�B
g�B
g�B
h>B
hXB
h�B
hsB
h�B
h�B
iB
iB
i�B
jeB
j�B
j�B
j�B
j�B
kB
j�B
k6B
kkB
k�B
k�B
l"B
l=B
l�B
l�B
mCB
mCB
mwB
m]B
mwB
m�B
nB
nB
n/B
n/B
nIB
ncB
n�B
n�B
oB
oOB
oiB
o�B
o�B
o�B
o�B
pB
p;B
p;B
p;B
p�B
p�B
p�B
qB
qB
q�B
q[B
q�B
q�B
q�B
r-B
raB
r-B
r�B
r�B
r�B
s3B
shB
sMB
sMB
s�B
tB
tB
tB
t�B
t�B
t�B
t�B
t�B
t�B
uB
u%B
u?B
u�B
u�B
vB
u�B
vB
v`B
vzB
v�B
v�B
v�B
v�B
wLB
wfB
wfB
w�B
w�B
xB
xRB
xlB
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y	B
yXB
y�B
y�B
zB
z*B
z*B
z^B
z�B
z�B
z�B
z�B
z�B
{B
{JB
{dB
{B
{�B
{�B
{�B
{�B
|B
|6B
|6B
|jB
|�B
|�B
|�B
|�B
|�B
|�B
|�B
}<B
}VB
}qB
}�B
~(B
~]B
~wB
~�B
~�B
~�B
cB
cB
�B
�B
� B
��B
�iB
�iB
��B
�B
�UB
�UB
��B
��B
��B
�B
�'B
�[B
�AB
�uB
��B
��B
�-B
�GB
�GB
��B
�{B
��B
��B
��B
��B
�gB
�MB
��B
�B
�B
�mB
�B
�B
�B
�SB
��B
��B
�B
�YB
�?B
�%B
�%B
�YB
�?B
��B
��B
��B
��B
��B
��B
��B
�+B
�EB
�zB
�zB
��B
��B
��B
�KB
�fB
��B
��B
��B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B	>BB	>B	=�B	=VB	=VB	=qB	=<B	=VB	=qB	=qB	=VB	=qB	=qB	<�B	=�B	=�B	=�B	=�B	>(B	>(B	>BB	>wB	>�B	>�B	>wB	>�B	>�B	>�B	>�B	>�B	>�B	?B	?HB	@ B	?�B	?�B	@�B	BAB	GB	L�B	R�B	q�B	�)B
:xB
N�B
��B
ݘB
�B
��BSB"hB$�B vB&LB;dBK^BY1B[�B]�B]dBX�BoiB~(B��B}VByrBl�Bg�BbNBI�B?�B<B:�B6B1ABN�BRTB88B
�CB
��B
vB
P�B
@4B
?�B
8�B
%FB	�jB	͟B	��B	��B	��B	x�B	kB	9	B	)B	yB	oB	
=B	 4B�qB�bB��BخBԕB� B�KB��B��B�|B�)B�B�ZB��B��B�^B��B�=B��B�YB��B��B�
B��B�(B�BB��B��B�pB��B��B�QB��B�GB��B�B��B�8B�9B�^BуB�_BܬB�nB�wB�B	UB	dB	(B	B	�B	!�B	 BB	$B	%�B	+�B	0!B	+�B	4TB	9�B	:�B	:�B	:�B	<PB	AoB	CaB	CGB	G_B	J�B	K^B	`�B	p�B	{JB	s�B	rB	|�B	�4B	��B	�GB	z�B	��B	��B	�B	�iB	|�B	xB	xB	v�B	xRB	z�B	��B	��B	�B	�B	�=B	��B	�EB	�lB	�^B	��B	��B	�NB	��B	��B	��B	�BB	��B	��B	�DB	��B	�HB	��B	�YB	��B	�B	��B	�sB	�B	�B	�1B	��B	��B	�gB	�B	�
B	��B	�mB	�YB	�yB	��B	�B	��B	��B	�]B	��B	�	B	��B	��B	�pB	�NB	��B	�fB	��B	��B	�B	�kB	�qB	�B	�B	��B	��B	��B	��B	��B	�rB	��B	�4B	�{B	ĶB	��B	��B	�B	ȀB	�+B	��B	�mB	�YB	�zB	�B	�KB	ȀB	��B	�=B	̘B	͹B	ΥB	�vB	�NB	� B	��B	҉B	�oB	�NB	�NB	��B	ѷB	��B	��B	�FB	ѷB	� B	��B	յB	�B	�B	׍B	چB	ۦB	��B	�xB	��B	�B	޸B	��B	߾B	ߤB	�HB	�B	�B	��B	��B	�B	�B	�-B	��B	�HB	��B	��B	�B	�B	�vB	��B	�B	��B	�B	��B	�B	�@B	�tB	�nB	��B	�2B	�`B	�FB	�B	�B	��B	��B	�B	�mB	�
B	�sB	�sB	�XB	�sB	��B	�eB	��B	�B	�B	��B	��B	�CB	��B	��B	�B	�B	��B	�B	��B	��B	�oB	�B	��B	�B	�?B	�?B	�ZB	��B	��B	��B	�LB	��B	�AB	�B	��B	��B	�`B	�?B	�%B	�%B	�ZB	��B	�FB	��B	�+B	�B	��B	�lB	�B	�	B	��B	�JB	��B	�B	�jB	��B	�"B	��B	�B	��B
�B
�B
+B
�B
�B
	RB
	�B
	lB

rB
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
JB
B
dB
~B
JB
dB
JB
dB
0B
0B
B
�B
�B
�B
VB
<B
<B
VB
VB
B
�B
�B
dB
JB
�B
�B
�B
dB
~B
�B
B
�B
BB
4B
�B
�B
&B
�B
2B
MB
�B
�B
SB
B
�B
�B
9B
B
eB
)B
B
�B
�B
�B
�B
�B
5B
5B
B
!B
 B
 vB
 vB
!HB
!�B
!�B
"4B
"hB
"�B
#nB
#�B
$&B
#�B
$@B
$ZB
$@B
$&B
%,B
&B
&LB
&fB
&fB
'8B
'mB
'mB
(
B
'�B
'�B
($B
(>B
(
B
($B
(�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
*eB
*�B
+B
+�B
+�B
+�B
+�B
+�B
,�B
-CB
-�B
-wB
-�B
-�B
./B
.�B
/5B
/iB
/�B
/�B
/�B
0!B
0�B
1�B
2GB
2�B
3B
3�B
3�B
4B
4B
49B
4�B
5B
5�B
5�B
5�B
5�B
5�B
6+B
5�B
6+B
6FB
6+B
5�B
5�B
5�B
5�B
5�B
5�B
5tB
5ZB
4�B
4�B
4�B
5�B
6B
6`B
6�B
8lB
7�B
88B
7�B
8lB
9	B
9XB
9rB
9�B
9XB
9�B
9�B
:xB
:DB
:�B
;dB
;0B
;dB
;�B
<B
<6B
<6B
<6B
<PB
<�B
<�B
<�B
=<B
>]B
>�B
>�B
?}B
@�B
A B
A B
A�B
A�B
A�B
A�B
B�B
C-B
C�B
C�B
D3B
DgB
D�B
EB
ESB
E�B
F?B
F�B
F�B
F�B
F�B
F�B
F�B
GB
G_B
GzB
G�B
GzB
F�B
G_B
GEB
G�B
G�B
G�B
HB
G�B
HB
IB
I�B
I�B
I�B
I�B
I�B
I�B
J#B
J�B
K^B
K�B
K�B
L0B
MB
MPB
MPB
M�B
MPB
MjB
N�B
OB
OvB
OBB
OvB
O�B
O\B
O�B
PB
PbB
P}B
P�B
QB
QhB
QhB
Q�B
RoB
R�B
R�B
R�B
SB
TFB
TaB
T�B
T{B
T�B
UgB
UMB
U�B
UgB
UgB
U�B
U�B
VB
VSB
V�B
V�B
WYB
W�B
W�B
XB
W�B
X�B
X�B
Y�B
YeB
ZB
Z�B
Z�B
Z�B
Z�B
[	B
[	B
[	B
Z�B
[	B
[qB
[WB
[	B
[�B
[�B
[�B
[�B
\]B
\�B
\�B
]B
]dB
]dB
]IB
]/B
]B
]�B
^B
^5B
^�B
_B
_B
_VB
`'B
`�B
`�B
aB
a�B
a�B
a�B
bB
bNB
bhB
b�B
b�B
c�B
d&B
c�B
d�B
d�B
eB
e,B
e`B
eFB
ezB
fLB
fLB
f2B
f2B
f�B
f�B
f�B
f�B
f�B
f�B
gRB
g�B
h
B
h
B
g�B
g�B
g�B
h>B
hXB
h�B
hsB
h�B
h�B
iB
iB
i�B
jeB
j�B
j�B
j�B
j�B
kB
j�B
k6B
kkB
k�B
k�B
l"B
l=B
l�B
l�B
mCB
mCB
mwB
m]B
mwB
m�B
nB
nB
n/B
n/B
nIB
ncB
n�B
n�B
oB
oOB
oiB
o�B
o�B
o�B
o�B
pB
p;B
p;B
p;B
p�B
p�B
p�B
qB
qB
q�B
q[B
q�B
q�B
q�B
r-B
raB
r-B
r�B
r�B
r�B
s3B
shB
sMB
sMB
s�B
tB
tB
tB
t�B
t�B
t�B
t�B
t�B
t�B
uB
u%B
u?B
u�B
u�B
vB
u�B
vB
v`B
vzB
v�B
v�B
v�B
v�B
wLB
wfB
wfB
w�B
w�B
xB
xRB
xlB
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y	B
yXB
y�B
y�B
zB
z*B
z*B
z^B
z�B
z�B
z�B
z�B
z�B
{B
{JB
{dB
{B
{�B
{�B
{�B
{�B
|B
|6B
|6B
|jB
|�B
|�B
|�B
|�B
|�B
|�B
|�B
}<B
}VB
}qB
}�B
~(B
~]B
~wB
~�B
~�B
~�B
cB
cB
�B
�B
� B
��B
�iB
�iB
��B
�B
�UB
�UB
��B
��B
��B
�B
�'B
�[B
�AB
�uB
��B
��B
�-B
�GB
�GB
��B
�{B
��B
��B
��B
��B
�gB
�MB
��B
�B
�B
�mB
�B
�B
�B
�SB
��B
��B
�B
�YB
�?B
�%B
�%B
�YB
�?B
��B
��B
��B
��B
��B
��B
��B
�+B
�EB
�zB
�zB
��B
��B
��B
�KB
�fB
��B
��B
��B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20230228034333  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8b                                                                20230228034334  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20230228034335  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20230228034335                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20230228034336  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20230228034336  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20230228040047                      G�O�G�O�G�O�                