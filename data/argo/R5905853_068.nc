CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:34:47Z creation;2022-06-04T17:34:48Z conversion to V3.1      
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
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20220604173447  20220610131507  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               DA   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @�O1@�t1   @�O1����@,;dZ��cG-1   GPS     A   B   B   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8ffB@  BG��BPffBV  Bb  BfffBp  Bx  B�  B�  B�  B�33B���B���B�  B�  B�  B�  B���B�  B�  B�33B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C�C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D �fD!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ�fDR  DR� DS  DS� DT  DT� DU  DU� DV  DVy�DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @p�@s�
@��@��A��A<��A\��A|��A�z�A�z�A�z�A�z�A�z�A�z�A�z�A�z�B=qB=qB=qB=qB'=qB/=qB7��B?=qBF�BO��BU=qBa=qBe��Bo=qBw=qB=qB���B���B���B�k�B�k�B���B���B���B���B�k�B���B���B���B���B�k�B���BÞ�BǞ�B˞�BϞ�BӞ�Bמ�B۞�Bߞ�B㞸B瞸B랸BB�B���B���B���C�\C�\C�\C�\C	�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C��C�\C!�\C#�\C%�\C'�\C)�\C+�\C-�\C/�\C1�\C3�\C5�\C7�\C9�\C;�\C=�\C?�\CA�\CC�\CE�\CG�\CI�\CK�\CM�\CO�\CQ�\CS�\CU�\CW�\CY�\C[�\C]�\C_�\Ca�\Cc�\Ce�\Cg�\Ci�\Ck�\Cm�\Co�\Cq�\Cs�\Cu�\Cw�\Cy�\C{�\C}�\C�\C��C��C��C��C��{C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��{C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��{C��C��C��{C��C��C��C��C��C��C��C��C��C��C��C��D s�D ��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��D	s�D	��D
s�D
��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��D z=D ��D!s�D!��D"s�D"��D#s�D#��D$s�D$��D%s�D%��D&s�D&��D's�D'��D(s�D(��D)s�D)��D*s�D*��D+s�D+��D,s�D,��D-s�D-��D.s�D.��D/s�D/��D0s�D0��D1s�D1��D2s�D2��D3s�D3��D4s�D4��D5s�D5��D6s�D6��D7s�D7��D8s�D8��D9s�D9��D:s�D:��D;s�D;��D<s�D<��D=s�D=��D>s�D>��D?s�D?��D@s�D@��DAs�DA��DBs�DB��DCs�DC��DDs�DD��DEs�DE��DFs�DF��DGs�DG��DHs�DH��DIs�DI��DJs�DJ��DKs�DK��DLs�DL��DMs�DM��DNs�DN��DOs�DO��DPs�DP��DQz=DQ��DRs�DR��DSs�DS��DTs�DT��DUs�DU��DVmqDV��DWs�DW��DXs�DX��DYs�DY��DZs�DZ��D[s�D[��D\s�D\��D]s�D]��D^s�D^��D_s�D_��D`s�D`��Das�Da��Dbs�Db��Dcs�Dc��Dds�Dd��Des�De��Dfs�Df��Dgs�Dg��Dhs�Dh��Dis�Di��Djs�Dj��Dks�Dk��Dls�Dl��Dms�Dm��Dns�Dn��Dos�Do��Dps�Dp��Dqs�Dq��Drs�Dr��Dss�Ds��Dts�Dt��Dus�Du��Dvs�Dv��Dws�Dw��Dxs�Dx��Dys�Dy��Dzs�Dz��D{s�D{��D|s�D|��D}s�D}��D~s�D~��Ds�D��D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D¹�D���D�9�D�y�Dù�D���D�9�D�y�DĹ�D���D�9�D�y�DŹ�D���D�9�D�y�Dƹ�D���D�9�D�y�Dǹ�D���D�9�D�y�Dȹ�D���D�9�D�y�Dɹ�D���D�9�D�y�Dʹ�D���D�9�D�y�D˹�D���D�9�D�y�D̹�D���D�9�D�y�D͹�D���D�9�D�y�Dι�D���D�9�D�y�DϹ�D���D�9�D�y�Dй�D���D�9�D�y�Dѹ�D���D�9�D�y�Dҹ�D���D�9�D�y�Dӹ�D���D�9�D�y�DԹ�D���D�9�D�y�Dչ�D���D�9�D�y�Dֹ�D���D�9�D�y�D׹�D���D�9�D�y�Dع�D���D�9�D�y�Dٹ�D���D�9�D�y�Dڹ�D���D�9�D�y�D۹�D���D�9�D�y�Dܹ�D���D�9�D�y�Dݹ�D���D�9�D�y�D޹�D���D�9�D�y�D߹�D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D�D���D�9�D�y�D��D���D�9�D�y�D���D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�6�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��MA��cA�A��A��A��A��A��A���A��JA�.�A�7�A�4�A�7�A�;�A�8A�>BA�@A�@�A�A�A�B�A�C�A��A��A��aAǥzA��A�c�A�LdA��A��A���A��A�1A���A��A~~�A{�As��Al!�Aj��AfیAb�!Aa;�A^v`AUVmAMP�AJ�[AI>�AG��AE�YADxAC�hAB�sABAA<�A?PHA>�'A=X�A;��A:��A9�A8.IA6��A5�A5j�A5s�A5�'A6VA5��A5OA5&�A5(�A4�.A4�wA3�}A2h�A1[WA0&A/�A.�A-�4A-�A,/�A+�A,&�A,;A+�9A+�_A+6�A*��A*!�A)�hA(S�A'��A'9XA&+A%^�A$�A$�UA$�kA$ffA#�/A#A"F�A"+A!�A! �A AԕAo�ACA��A6AeAkQA�|AZ�A��A��A�A]�A�'A��A�'A�AuA�4Al�A=A��A�OA�A�A�<A*�A�hA�A�A]�A�FA
hsA	��A�CAA&�AB�A)_A�A�A�oA��A�3ATaA,=A�[Ai�A��A?AQ�APHA5�AȴA~�A:*A�A��A��A�{A=�AzA�A m�A n/A Z@��@�w2@�֡@�ff@��&@�]�@�y�@��"@�b�@��@���@���@���@��+@�N�@��#@���@�+�@���@�g8@��@�B[@���@�Z�@���@�7@��"@�@�-@�,�@�@�:*@@�D@�f@�;@��@�	@�ߤ@�J@�hs@��@�A�@��@��@��@��d@�\�@�{�@�� @�8@���@�$�@��@���@ߨX@�s@�@O@��@�Q@ݒ:@�҉@�C-@��@�IR@ړu@ٲ�@�5�@ؤ�@�I�@��@�ϫ@֣@��@չ�@��@Ԟ�@�-@��@�@қ�@ю�@��@��v@�x�@��@�[�@ͼ@���@̶�@� �@��
@ˏ�@�
=@�|�@��@�m]@��@�ѷ@Ȗ�@��@�e�@��@ƾ@�A�@�ƨ@�T�@�@@ĸR@�@�a�@��@��]@��@�@�V@��M@�Ĝ@Y@�Z@�Q@�8�@��@�hs@�S�@�<6@��b@� �@�Vm@���@�>B@�@�]�@��@�@�@��;@�5�@���@�g8@��*@�$t@��y@�Q@���@�*0@���@��@�@�PH@��W@��6@��@��o@�zx@�.I@���@��@�O�@���@��_@���@�Y�@���@�͟@��@��b@��1@�Z�@���@��F@�|�@�
=@��.@�͟@��@�M�@��r@���@�iD@�%@�|�@�_@��@��4@�N<@�9�@��M@��\@�l�@�#:@��w@�o @�"�@���@��}@�c�@�1@���@��<@�Ov@��@�x@��+@�ݘ@���@��@��@���@�zx@��f@��f@�;d@�f�@�{J@�b�@�0�@��Y@���@�<6@��@�p;@�	@��@��o@���@�X�@��@���@���@�V�@�
�@��j@��@���@��w@��@��@�v`@�f�@��@��]@���@�kQ@��@��@�x�@�#�@���@�U2@�9X@�	�@��@���@�J#@���@��@�8�@�k�@�1�@��@�֡@��_@�Ov@��)@���@���@�E9@��@���@�y>@�7�@���@�l�@�F@�:�@��@�@��@��.@�@��N@��4@�=�@�C@�;@�ی@��\@�Z�@�+k@��@��@��a@���@�[W@�:�@���@��F@�9X@�˒@���@���@�˒@��q@��f@�|�@�J�@��@�a|@�$�@���@��
@���@�E9@�ѷ@���@��@�6@��@���@��@��V@�x@�O�@�֡@�x@��F@���@�<6@�ں@���@�-�@��@��@��@��@�
=@��`@��9@�%�@��]@��@�G�@��@�kQ@��@4�@~��@~q�@}a�@|�`@|֡@|��@|�Y@|%�@{.I@zz@y�=@yT�@x�@xw�@w�}@v�@v3�@u�@u�@u�@u}�@u%F@ttT@s��@s�P@s.I@r�H@r�R@rz@re@q�N@q�S@q�@p��@p��@p�@oRT@n��@n��@nd�@m��@m�z@m��@ms�@m=�@l�I@l`�@l7�@lx@k˒@k|�@k6z@j�]@jff@i��@i@h��@h_@h!@g��@gE9@f��@f�}@fu%@fQ@e�@e�C@ek�@d�K@dr�@c��@cqv@c1�@b�8@b�B@b�R@b��@b=q@a�@a��@aT�@a�@`�@`��@`�.@`M@_ƨ@_|�@_�@^��@^��@^J�@]@]f�@\ѷ@\�.@\'R@\G@[��@[��@[�6@[��@Z�'@Z�@Y��@X�[@WZ�@Vxl@V-@V_@U�@U��@Tی@Tu�@Tm�@TQ�@Sݘ@S�F@S�@@S�V@S��@SE9@Rں@R�@R�}@Ri�@R�@R_@R_@Q�Z@Q�S@Qs�@P��@Pj@PG@O�q@O>�@N��@N��@N��@N;�@N!�@M��@M��@M��@M��@M%F@L�@Ll"@LU2@L%�@L@K˒@K&@J��@I��@I:�@H�@H�j@H��@Hr�@H`�@H1'@G~�@FkQ@E�@Ex�@Ec�@E+@D��@D'R@D�@D�@D�@C��@C��@C�:@C,�@B��@Bu@A��@Aa�@A2a@A�@@y>@@6@@�@?�@?iD@?�@>��@=�@=\�@=;@<�j@<bN@;�@;=@:�X@:��@:�\@:l�@:i�@:W�@:Z�@:Ov@:�@9�@8��@8<�@7��@7��@7$t@6��@6W�@6@5��@5��@5Y�@5 \@4�@4�9@4r�@3�@3��@3�F@3dZ@2�@2�L@2\�@2M�@2�@1��@1��@1B�@0�@0M@/�:@/x@/a@/J#@/'�@.�m@.�r@.E�@-�9@-��@-%@, �@+)_@*��@*�A@*4@)��@)^�@)	l@(�U@(�@(x@'�4@'.I@&��@&�@$��@#�@#�Q@#��@#��@#n/@#$t@"�6@"4@!�@!�~@!f�@!�@ �I@ A�@�6@E9@"�@��@�,@��@s�@^5@;�@J@��@��@T�@@@�)@`�@b@�A@��@�@v`@6z@�h@a|@O@�@�@�H@�t@hs@N<@ \@��@l"@6@@�W@�K@\)@��@n�@h
@\�@8�@��@�^@�"@�M@x�@+�@�O@�@H@�*@��@_p@)_@�@�@�B@�@��@�R@��@M�@�@��@�@��@�N@�t@c@Dg@(�@��@�@�I@4n@�}@�4@g�@a@dZ@O@�8@u%@O@ԕ@�3@�@B�@5�@�@�P@��@�9@u�@,=@x@��@�&@��@�@�@��@�[@�*@��@�@~�@y�@qv@j�@X�@>�@4�@1�@
��@
�\@
?@
+k@
�@	��@	��@	�@	|@	\�@	Q�@	<6@	0�@	#�@	;@��@�5@�e@�@�@�o@tT@[�@:�@�@  @��@_p@'�@(@�,@��@�x@�F@��@��@�F@�F@�r@n�@:*@��@�d@��@m]@/@@@�@�@V@V@�@�@�?@�@w�@h�@[�@%�@�@�@�Q@�
@��@��@�6@�w@��@��@�4@\)@�@��@�@�@z@xl@v�@v�@xl@q�@a|@V@e@�N@��@�~@�@s�111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��MA��cA�A��A��A��A��A��A���A��JA�.�A�7�A�4�A�7�A�;�A�8A�>BA�@A�@�A�A�A�B�A�C�A��A��A��aAǥzA��A�c�A�LdA��A��A���A��A�1A���A��A~~�A{�As��Al!�Aj��AfیAb�!Aa;�A^v`AUVmAMP�AJ�[AI>�AG��AE�YADxAC�hAB�sABAA<�A?PHA>�'A=X�A;��A:��A9�A8.IA6��A5�A5j�A5s�A5�'A6VA5��A5OA5&�A5(�A4�.A4�wA3�}A2h�A1[WA0&A/�A.�A-�4A-�A,/�A+�A,&�A,;A+�9A+�_A+6�A*��A*!�A)�hA(S�A'��A'9XA&+A%^�A$�A$�UA$�kA$ffA#�/A#A"F�A"+A!�A! �A AԕAo�ACA��A6AeAkQA�|AZ�A��A��A�A]�A�'A��A�'A�AuA�4Al�A=A��A�OA�A�A�<A*�A�hA�A�A]�A�FA
hsA	��A�CAA&�AB�A)_A�A�A�oA��A�3ATaA,=A�[Ai�A��A?AQ�APHA5�AȴA~�A:*A�A��A��A�{A=�AzA�A m�A n/A Z@��@�w2@�֡@�ff@��&@�]�@�y�@��"@�b�@��@���@���@���@��+@�N�@��#@���@�+�@���@�g8@��@�B[@���@�Z�@���@�7@��"@�@�-@�,�@�@�:*@@�D@�f@�;@��@�	@�ߤ@�J@�hs@��@�A�@��@��@��@��d@�\�@�{�@�� @�8@���@�$�@��@���@ߨX@�s@�@O@��@�Q@ݒ:@�҉@�C-@��@�IR@ړu@ٲ�@�5�@ؤ�@�I�@��@�ϫ@֣@��@չ�@��@Ԟ�@�-@��@�@қ�@ю�@��@��v@�x�@��@�[�@ͼ@���@̶�@� �@��
@ˏ�@�
=@�|�@��@�m]@��@�ѷ@Ȗ�@��@�e�@��@ƾ@�A�@�ƨ@�T�@�@@ĸR@�@�a�@��@��]@��@�@�V@��M@�Ĝ@Y@�Z@�Q@�8�@��@�hs@�S�@�<6@��b@� �@�Vm@���@�>B@�@�]�@��@�@�@��;@�5�@���@�g8@��*@�$t@��y@�Q@���@�*0@���@��@�@�PH@��W@��6@��@��o@�zx@�.I@���@��@�O�@���@��_@���@�Y�@���@�͟@��@��b@��1@�Z�@���@��F@�|�@�
=@��.@�͟@��@�M�@��r@���@�iD@�%@�|�@�_@��@��4@�N<@�9�@��M@��\@�l�@�#:@��w@�o @�"�@���@��}@�c�@�1@���@��<@�Ov@��@�x@��+@�ݘ@���@��@��@���@�zx@��f@��f@�;d@�f�@�{J@�b�@�0�@��Y@���@�<6@��@�p;@�	@��@��o@���@�X�@��@���@���@�V�@�
�@��j@��@���@��w@��@��@�v`@�f�@��@��]@���@�kQ@��@��@�x�@�#�@���@�U2@�9X@�	�@��@���@�J#@���@��@�8�@�k�@�1�@��@�֡@��_@�Ov@��)@���@���@�E9@��@���@�y>@�7�@���@�l�@�F@�:�@��@�@��@��.@�@��N@��4@�=�@�C@�;@�ی@��\@�Z�@�+k@��@��@��a@���@�[W@�:�@���@��F@�9X@�˒@���@���@�˒@��q@��f@�|�@�J�@��@�a|@�$�@���@��
@���@�E9@�ѷ@���@��@�6@��@���@��@��V@�x@�O�@�֡@�x@��F@���@�<6@�ں@���@�-�@��@��@��@��@�
=@��`@��9@�%�@��]@��@�G�@��@�kQ@��@4�@~��@~q�@}a�@|�`@|֡@|��@|�Y@|%�@{.I@zz@y�=@yT�@x�@xw�@w�}@v�@v3�@u�@u�@u�@u}�@u%F@ttT@s��@s�P@s.I@r�H@r�R@rz@re@q�N@q�S@q�@p��@p��@p�@oRT@n��@n��@nd�@m��@m�z@m��@ms�@m=�@l�I@l`�@l7�@lx@k˒@k|�@k6z@j�]@jff@i��@i@h��@h_@h!@g��@gE9@f��@f�}@fu%@fQ@e�@e�C@ek�@d�K@dr�@c��@cqv@c1�@b�8@b�B@b�R@b��@b=q@a�@a��@aT�@a�@`�@`��@`�.@`M@_ƨ@_|�@_�@^��@^��@^J�@]@]f�@\ѷ@\�.@\'R@\G@[��@[��@[�6@[��@Z�'@Z�@Y��@X�[@WZ�@Vxl@V-@V_@U�@U��@Tی@Tu�@Tm�@TQ�@Sݘ@S�F@S�@@S�V@S��@SE9@Rں@R�@R�}@Ri�@R�@R_@R_@Q�Z@Q�S@Qs�@P��@Pj@PG@O�q@O>�@N��@N��@N��@N;�@N!�@M��@M��@M��@M��@M%F@L�@Ll"@LU2@L%�@L@K˒@K&@J��@I��@I:�@H�@H�j@H��@Hr�@H`�@H1'@G~�@FkQ@E�@Ex�@Ec�@E+@D��@D'R@D�@D�@D�@C��@C��@C�:@C,�@B��@Bu@A��@Aa�@A2a@A�@@y>@@6@@�@?�@?iD@?�@>��@=�@=\�@=;@<�j@<bN@;�@;=@:�X@:��@:�\@:l�@:i�@:W�@:Z�@:Ov@:�@9�@8��@8<�@7��@7��@7$t@6��@6W�@6@5��@5��@5Y�@5 \@4�@4�9@4r�@3�@3��@3�F@3dZ@2�@2�L@2\�@2M�@2�@1��@1��@1B�@0�@0M@/�:@/x@/a@/J#@/'�@.�m@.�r@.E�@-�9@-��@-%@, �@+)_@*��@*�A@*4@)��@)^�@)	l@(�U@(�@(x@'�4@'.I@&��@&�@$��@#�@#�Q@#��@#��@#n/@#$t@"�6@"4@!�@!�~@!f�@!�@ �I@ A�@�6@E9@"�@��@�,@��@s�@^5@;�@J@��@��@T�@@@�)@`�@b@�A@��@�@v`@6z@�h@a|@O@�@�@�H@�t@hs@N<@ \@��@l"@6@@�W@�K@\)@��@n�@h
@\�@8�@��@�^@�"@�M@x�@+�@�O@�@H@�*@��@_p@)_@�@�@�B@�@��@�R@��@M�@�@��@�@��@�N@�t@c@Dg@(�@��@�@�I@4n@�}@�4@g�@a@dZ@O@�8@u%@O@ԕ@�3@�@B�@5�@�@�P@��@�9@u�@,=@x@��@�&@��@�@�@��@�[@�*@��@�@~�@y�@qv@j�@X�@>�@4�@1�@
��@
�\@
?@
+k@
�@	��@	��@	�@	|@	\�@	Q�@	<6@	0�@	#�@	;@��@�5@�e@�@�@�o@tT@[�@:�@�@  @��@_p@'�@(@�,@��@�x@�F@��@��@�F@�F@�r@n�@:*@��@�d@��@m]@/@@@�@�@V@V@�@�@�?@�@w�@h�@[�@%�@�@�@�Q@�
@��@��@�6@�w@��@��@�4@\)@�@��@�@�@z@xl@v�@v�@xl@q�@a|@V@e@�N@��@�~@�@s�111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�B��B�dB�)BʌB�B�~BɆB�EBǮBөB�sB��B֡B�B�mB��BؓB��B��B�eB�B��B�B_�BR:B��B	�"B	�oB	�B	�B	�?B	�*B	��B	��B	�SB	p�B	dB	UB	:�B	1[B	4�B	/5B	0!B	+�B	aB	�B	�B	�B	�B	#:B	(�B	*�B	1B	6�B	<�B	CGB	D�B	MjB	_�B	lqB	s�B	�QB	�B	�B	�hB	ٚB	�'B	��B	��B	�B	�B
�B
KB
%�B
,"B
&�B
"4B
 �B
#:B
(�B
1�B
5�B
2-B
4B
B�B
OB
O�B
P.B
PbB
O�B
PHB
Q B
TB
S[B
RTB
P.B
P�B
P�B
Q4B
Q B
P.B
OBB
N�B
L0B
K�B
J=B
HKB
G�B
E�B
C�B
B[B
AB
?HB
:�B
6FB
49B
0�B
0�B
/�B
0;B
.}B
-CB
+�B
*�B
)�B
*0B
+�B
0�B
1vB
0�B
/�B
,�B
+B
*�B
6�B
*B
%�B
sB
;B	�B	�WB	��B	�FB	�nB	��B	�B	�B	�UB	�UB	�!B	�B	�B	�B	�B	�B	�B	��B	�)B	�IB	�OB	�B	��B	�oB	�B	� B	�cB	�/B	��B	�wB	�qB	�KB	��B	��B	�qB	��B	�qB	�B	��B	�eB	�B	��B
UB
YB
	7B
�B
�B
�B
?B
�B
�B
�B
B
gB
aB
�B
 B	�wB	��B	�B	�lB	�B	��B	�iB	��B	��B	��B	�B	�B	��B	ޞB	��B	�xB	��B	� B	�tB	�`B	�B	�2B	��B	��B	�LB	�B	�,B	�ZB	�&B	�TB	��B	�B	�|B	��B	�pB	�!B	ބB	�~B	��B	��B	�B	��B	�IB	ݘB	�B	ޞB	߾B	��B	�B	�:B	�,B	��B	��B	��B	�B	��B	�CB	�B	�iB	��B	�qB	��B	�WB	�B	��B	��B	�qB	�=B	�B	�B	�B	�B	�6B	�B	�kB	�B	�WB	�]B	��B	�B	�]B	�B	�IB	�5B	�5B	� B	�B	�B	�B	�B	��B	�"B	�}B	��B
 B
UB
�B
�B
uB
�B
�B
B
�B
YB
�B
_B
zB
_B
zB
�B
YB
�B
%B
%B
YB
�B
?B
�B
�B
�B
�B
[B
�B
'B
UB
�B
B
�B

�B
	�B
�B
B
B
9B
�B
�B
�B
3B
aB
�B
B
YB
�B
1B
KB
	7B
	�B
	�B
	�B
	lB
	lB
�B
�B
�B
�B
6B
6B
B
B
�B
xB
DB

�B
�B
�B

�B

=B
	�B
�B
fB
�B
�B
�B
B
�B
�B
�B
�B
+B
�B
KB
fB
1B
fB
KB
�B

	B

�B

�B
)B
�B
�B
B
hB
bB
�B
�B
�B
B
"B
�B
(B
}B
4B
�B
:B
&B
B
�B
yB
�B
�B
B
KB
eB
eB
B
B
QB
�B
�B
=B
qB
�B
�B
CB
xB
�B
B
�B
OB
!B
�B
 B
 �B
!HB
!HB
!-B
 �B
 �B
 vB
 'B
�B
 B
 B
 BB
 'B
 \B
 �B
!-B
!�B
!�B
!�B
"B
"4B
"4B
"�B
"�B
"4B
!�B
!|B
!HB
!-B
 �B
 �B
 �B
 �B
!|B
!�B
!|B
 �B
 �B
!�B
"�B
"�B
"�B
#�B
%�B
)B
)�B
)�B
)�B
*KB
*�B
+B
,qB
,=B
,qB
,�B
,�B
-�B
-�B
-�B
-�B
.IB
./B
.�B
.�B
.�B
.�B
.}B
/�B
0oB
0�B
0�B
1'B
1AB
1[B
2GB
2-B
2-B
2-B
2GB
3MB
3MB
3hB
4TB
4nB
4B
5B
5%B
5tB
5�B
6`B
6`B
6�B
7fB
7�B
7�B
7�B
7�B
7�B
88B
8�B
9>B
9>B
9�B
9�B
:B
:�B
;JB
;dB
;JB
;JB
;JB
;�B
<B
<PB
<PB
<�B
<�B
<�B
<�B
=B
<�B
="B
=�B
=�B
=�B
>(B
>�B
>�B
>�B
?.B
?�B
?�B
?�B
?�B
@4B
@�B
@�B
@�B
A B
A B
A;B
A�B
A�B
A�B
B�B
B�B
CGB
CaB
CaB
DB
D3B
D�B
D�B
D�B
D�B
EB
EB
E9B
EmB
E�B
F�B
F�B
F�B
GB
GB
G+B
GEB
G_B
G�B
G�B
G�B
H1B
HKB
H1B
H�B
H�B
IB
IRB
I�B
I�B
I�B
J#B
J�B
K)B
K�B
K�B
LdB
LdB
L~B
L~B
LJB
LJB
MB
MB
L�B
L�B
LJB
LdB
LdB
LdB
L0B
L0B
L~B
L�B
L�B
L�B
MPB
MB
MB
MB
MB
MPB
M�B
M�B
M�B
M�B
N"B
NB
NB
NB
NVB
N"B
N�B
N�B
OB
O\B
O�B
P.B
P�B
PbB
PbB
PHB
PbB
PbB
P}B
P�B
P�B
QNB
Q�B
Q�B
Q�B
Q�B
Q�B
R:B
R�B
R�B
SB
S@B
S@B
S[B
S[B
S[B
SB
S�B
T,B
T�B
TaB
TFB
TaB
T�B
UMB
UMB
U2B
UMB
U2B
UgB
UMB
U�B
U�B
V�B
V�B
W$B
V�B
W
B
XB
XB
X+B
X_B
X�B
YB
YB
Z7B
ZkB
Z�B
Z�B
[#B
[�B
\B
\xB
\]B
\xB
\�B
\�B
\�B
\xB
\]B
\�B
]~B
]~B
]�B
^B
]�B
^�B
^�B
_;B
_VB
_�B
_�B
_�B
`'B
`'B
`B
`BB
`�B
`�B
aB
aHB
bB
b4B
b�B
b�B
b�B
b�B
b�B
b�B
bNB
c B
c�B
c�B
d&B
dZB
d�B
eB
eFB
e`B
e�B
e�B
f�B
f�B
h
B
g�B
h>B
h�B
h�B
i*B
i*B
iyB
i�B
i�B
jKB
jB
kB
k6B
l�B
mB
l�B
l�B
l�B
m)B
mCB
m�B
nIB
ncB
n}B
n}B
n�B
oB
oiB
o�B
p;B
p;B
poB
poB
p�B
p�B
p�B
qB
q'B
q[B
q�B
q�B
q�B
rB
r|B
r�B
r�B
r�B
r�B
s3B
shB
s�B
tB
tTB
t�B
t�B
t�B
t�B
uB
uB
t�B
utB
u�B
u�B
vB
v+B
vFB
v�B
wB
wLB
wLB
w2B
wfB
w�B
w�B
w�B
w�B
w�B
xB
xlB
x�B
x�B
y�B
y�B
y�B
y�B
y�B
zDB
z^B
zxB
zDB
zxB
z�B
z�B
{0B
{dB
{JB
{dB
{B
{�B
|PB
|�B
|�B
|�B
|�B
}B
|�B
}"B
}�B
}�B
}�B
}�B
}�B
~BB
~�B
B
cB
HB
�B
� B
� B
�4B
�OB
�4B
��B
��B
�UB
�oB
��B
��B
��B
��B
��B
��B
��B
��B
�B
�AB
�'B
�AB
�AB
�AB
�[B
��B
��B
��B
��B
�-B
��B
��B
��B
��B
��B
��B
�gB
��B
��B
��B
��B
��B
�B
�B
�B
��B
��B
��B
��B
��B
��B
�%B
�?B
�YB
��B
�B
�zB
�zB
��B
��B
�B
�B
�1B
�B
�B
�B
�1B
�fB
��B
��B
�B
�B
��B
�	B
�=B
�=B
�=B
�=B
�=B
�#B
�XB
��B
�B
�)B
�DB
�^B
��B
��B
��B
�B
�B
��B
�B
��B
�JB
�0B
�dB
�dB
��B
�B
�6B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�VB
��B
��B
��B
�(B
�(111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�B��B�dB�)BʌB�B�~BɆB�EBǮBөB�sB��B֡B�B�mB��BؓB��B��B�eB�B��B�B_�BR:B��B	�"B	�oB	�B	�B	�?B	�*B	��B	��B	�SB	p�B	dB	UB	:�B	1[B	4�B	/5B	0!B	+�B	aB	�B	�B	�B	�B	#:B	(�B	*�B	1B	6�B	<�B	CGB	D�B	MjB	_�B	lqB	s�B	�QB	�B	�B	�hB	ٚB	�'B	��B	��B	�B	�B
�B
KB
%�B
,"B
&�B
"4B
 �B
#:B
(�B
1�B
5�B
2-B
4B
B�B
OB
O�B
P.B
PbB
O�B
PHB
Q B
TB
S[B
RTB
P.B
P�B
P�B
Q4B
Q B
P.B
OBB
N�B
L0B
K�B
J=B
HKB
G�B
E�B
C�B
B[B
AB
?HB
:�B
6FB
49B
0�B
0�B
/�B
0;B
.}B
-CB
+�B
*�B
)�B
*0B
+�B
0�B
1vB
0�B
/�B
,�B
+B
*�B
6�B
*B
%�B
sB
;B	�B	�WB	��B	�FB	�nB	��B	�B	�B	�UB	�UB	�!B	�B	�B	�B	�B	�B	�B	��B	�)B	�IB	�OB	�B	��B	�oB	�B	� B	�cB	�/B	��B	�wB	�qB	�KB	��B	��B	�qB	��B	�qB	�B	��B	�eB	�B	��B
UB
YB
	7B
�B
�B
�B
?B
�B
�B
�B
B
gB
aB
�B
 B	�wB	��B	�B	�lB	�B	��B	�iB	��B	��B	��B	�B	�B	��B	ޞB	��B	�xB	��B	� B	�tB	�`B	�B	�2B	��B	��B	�LB	�B	�,B	�ZB	�&B	�TB	��B	�B	�|B	��B	�pB	�!B	ބB	�~B	��B	��B	�B	��B	�IB	ݘB	�B	ޞB	߾B	��B	�B	�:B	�,B	��B	��B	��B	�B	��B	�CB	�B	�iB	��B	�qB	��B	�WB	�B	��B	��B	�qB	�=B	�B	�B	�B	�B	�6B	�B	�kB	�B	�WB	�]B	��B	�B	�]B	�B	�IB	�5B	�5B	� B	�B	�B	�B	�B	��B	�"B	�}B	��B
 B
UB
�B
�B
uB
�B
�B
B
�B
YB
�B
_B
zB
_B
zB
�B
YB
�B
%B
%B
YB
�B
?B
�B
�B
�B
�B
[B
�B
'B
UB
�B
B
�B

�B
	�B
�B
B
B
9B
�B
�B
�B
3B
aB
�B
B
YB
�B
1B
KB
	7B
	�B
	�B
	�B
	lB
	lB
�B
�B
�B
�B
6B
6B
B
B
�B
xB
DB

�B
�B
�B

�B

=B
	�B
�B
fB
�B
�B
�B
B
�B
�B
�B
�B
+B
�B
KB
fB
1B
fB
KB
�B

	B

�B

�B
)B
�B
�B
B
hB
bB
�B
�B
�B
B
"B
�B
(B
}B
4B
�B
:B
&B
B
�B
yB
�B
�B
B
KB
eB
eB
B
B
QB
�B
�B
=B
qB
�B
�B
CB
xB
�B
B
�B
OB
!B
�B
 B
 �B
!HB
!HB
!-B
 �B
 �B
 vB
 'B
�B
 B
 B
 BB
 'B
 \B
 �B
!-B
!�B
!�B
!�B
"B
"4B
"4B
"�B
"�B
"4B
!�B
!|B
!HB
!-B
 �B
 �B
 �B
 �B
!|B
!�B
!|B
 �B
 �B
!�B
"�B
"�B
"�B
#�B
%�B
)B
)�B
)�B
)�B
*KB
*�B
+B
,qB
,=B
,qB
,�B
,�B
-�B
-�B
-�B
-�B
.IB
./B
.�B
.�B
.�B
.�B
.}B
/�B
0oB
0�B
0�B
1'B
1AB
1[B
2GB
2-B
2-B
2-B
2GB
3MB
3MB
3hB
4TB
4nB
4B
5B
5%B
5tB
5�B
6`B
6`B
6�B
7fB
7�B
7�B
7�B
7�B
7�B
88B
8�B
9>B
9>B
9�B
9�B
:B
:�B
;JB
;dB
;JB
;JB
;JB
;�B
<B
<PB
<PB
<�B
<�B
<�B
<�B
=B
<�B
="B
=�B
=�B
=�B
>(B
>�B
>�B
>�B
?.B
?�B
?�B
?�B
?�B
@4B
@�B
@�B
@�B
A B
A B
A;B
A�B
A�B
A�B
B�B
B�B
CGB
CaB
CaB
DB
D3B
D�B
D�B
D�B
D�B
EB
EB
E9B
EmB
E�B
F�B
F�B
F�B
GB
GB
G+B
GEB
G_B
G�B
G�B
G�B
H1B
HKB
H1B
H�B
H�B
IB
IRB
I�B
I�B
I�B
J#B
J�B
K)B
K�B
K�B
LdB
LdB
L~B
L~B
LJB
LJB
MB
MB
L�B
L�B
LJB
LdB
LdB
LdB
L0B
L0B
L~B
L�B
L�B
L�B
MPB
MB
MB
MB
MB
MPB
M�B
M�B
M�B
M�B
N"B
NB
NB
NB
NVB
N"B
N�B
N�B
OB
O\B
O�B
P.B
P�B
PbB
PbB
PHB
PbB
PbB
P}B
P�B
P�B
QNB
Q�B
Q�B
Q�B
Q�B
Q�B
R:B
R�B
R�B
SB
S@B
S@B
S[B
S[B
S[B
SB
S�B
T,B
T�B
TaB
TFB
TaB
T�B
UMB
UMB
U2B
UMB
U2B
UgB
UMB
U�B
U�B
V�B
V�B
W$B
V�B
W
B
XB
XB
X+B
X_B
X�B
YB
YB
Z7B
ZkB
Z�B
Z�B
[#B
[�B
\B
\xB
\]B
\xB
\�B
\�B
\�B
\xB
\]B
\�B
]~B
]~B
]�B
^B
]�B
^�B
^�B
_;B
_VB
_�B
_�B
_�B
`'B
`'B
`B
`BB
`�B
`�B
aB
aHB
bB
b4B
b�B
b�B
b�B
b�B
b�B
b�B
bNB
c B
c�B
c�B
d&B
dZB
d�B
eB
eFB
e`B
e�B
e�B
f�B
f�B
h
B
g�B
h>B
h�B
h�B
i*B
i*B
iyB
i�B
i�B
jKB
jB
kB
k6B
l�B
mB
l�B
l�B
l�B
m)B
mCB
m�B
nIB
ncB
n}B
n}B
n�B
oB
oiB
o�B
p;B
p;B
poB
poB
p�B
p�B
p�B
qB
q'B
q[B
q�B
q�B
q�B
rB
r|B
r�B
r�B
r�B
r�B
s3B
shB
s�B
tB
tTB
t�B
t�B
t�B
t�B
uB
uB
t�B
utB
u�B
u�B
vB
v+B
vFB
v�B
wB
wLB
wLB
w2B
wfB
w�B
w�B
w�B
w�B
w�B
xB
xlB
x�B
x�B
y�B
y�B
y�B
y�B
y�B
zDB
z^B
zxB
zDB
zxB
z�B
z�B
{0B
{dB
{JB
{dB
{B
{�B
|PB
|�B
|�B
|�B
|�B
}B
|�B
}"B
}�B
}�B
}�B
}�B
}�B
~BB
~�B
B
cB
HB
�B
� B
� B
�4B
�OB
�4B
��B
��B
�UB
�oB
��B
��B
��B
��B
��B
��B
��B
��B
�B
�AB
�'B
�AB
�AB
�AB
�[B
��B
��B
��B
��B
�-B
��B
��B
��B
��B
��B
��B
�gB
��B
��B
��B
��B
��B
�B
�B
�B
��B
��B
��B
��B
��B
��B
�%B
�?B
�YB
��B
�B
�zB
�zB
��B
��B
�B
�B
�1B
�B
�B
�B
�1B
�fB
��B
��B
�B
�B
��B
�	B
�=B
�=B
�=B
�=B
�=B
�#B
�XB
��B
�B
�)B
�DB
�^B
��B
��B
��B
�B
�B
��B
�B
��B
�JB
�0B
�dB
�dB
��B
�B
�6B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�VB
��B
��B
��B
�(B
�(111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104911  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604173447  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604173448  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604173448                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605023455  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605023455  QCF$                G�O�G�O�G�O�            4000JA  ARUP                                                                        20220610131507                      G�O�G�O�G�O�                