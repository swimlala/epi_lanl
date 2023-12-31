CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2017-10-19T00:35:13Z creation;2017-10-19T00:35:18Z conversion to V3.1;2019-12-19T07:58:56Z update;     
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
resolution        =���   axis      Z        D  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     D  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     D  _�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     D  r�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     D  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �@   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     D  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �X   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     D  �,   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     D  �p   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     D  ʴ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ڈ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �X   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �h   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �l   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �|   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20171019003513  20200115121517  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0576_170                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�.�uax 1   @�.�8� @:߱[W>��d�l�C��1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�  @�  A   A!��A@  Aa��A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B��B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C3�fC6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|�C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM�fDN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW�fDX  DX� DY  DY� DZ  DZ� D[  D[� D\fD\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D���D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�C3D� D�� D�  D�@ D� D�� D�  D�@ D�3D�� D�  D�C3D�vf11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��A�\A<��A^�\A|��A�z�A�z�A�z�A�z�A�z�A�z�A�z�A�z�B=qB=qB�
B=qB'=qB/=qB7=qB?=qBG=qBO=qBW=qB_=qBg=qBo=qBw=qB=qB���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���BÞ�BǞ�B˞�B�k�BӞ�Bמ�B۞�Bߞ�B㞸B瞸B랸BB�B���B���B���C�\C�\C�\C�\C	�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C!�\C#�\C%�\C'�\C)�\C+�\C-�\C/�\C1�\C3��C5�\C7�\C9�\C;�\C=�\C?�\CA�\CC�\CE�\CG�\CI�\CK�\CM�\CO�\CQ�\CS�\CU�\CW�\CY�\C[�\C]�\C_�\Ca�\Cc�\Ce�\Cg�\Ci�\Ck�\Cm�\Co�\Cq�\Cs�\Cu�\Cw�\Cy�\C{��C}�\C�\C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��{C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��{C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D s�D ��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��D	s�D	��D
s�D
��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��D s�D ��D!s�D!��D"s�D"��D#s�D#��D$s�D$��D%s�D%��D&s�D&��D's�D'��D(s�D(��D)s�D)��D*s�D*��D+s�D+��D,s�D,��D-s�D-��D.s�D.��D/s�D/��D0s�D0��D1s�D1��D2s�D2��D3s�D3��D4s�D4��D5s�D5��D6s�D6��D7s�D7��D8s�D8��D9s�D9��D:s�D:��D;s�D;��D<s�D<��D=s�D=��D>s�D>��D?s�D?��D@s�D@��DAs�DA��DBs�DB��DCs�DC��DDs�DD��DEs�DE��DFs�DF��DGs�DG��DHs�DH��DIs�DI��DJs�DJ��DKs�DK��DLs�DL��DMz=DM��DNs�DN��DOs�DO��DPs�DP��DQs�DQ��DRs�DR��DSs�DS��DTs�DT��DUs�DU��DVs�DV��DWz=DW��DXs�DX��DYs�DY��DZs�DZ��D[s�D[�=D\s�D\��D]s�D]��D^s�D^��D_s�D_��D`s�D`��Das�Da��Dbs�Db��Dcs�Dc��Dds�Dd��Des�De��Dfs�Df��Dgs�Dg��Dhs�Dh��Dis�Di��Djs�Dj��Dks�Dk��Dls�Dl��Dms�Dm��Dns�Dn��Dos�Do��Dps�Dp��Dqs�Dq��Drs�Dr��Dss�Ds��Dts�Dt��Dus�Du��Dvs�Dv��Dws�Dw��Dxs�Dx��Dys�Dy��Dzs�Dz��D{s�D{��D|s�D|��D}s�D}��D~s�D~��Ds�D��D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�=D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D��D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D��D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D¹�D���D�9�D�y�Dù�D���D�9�D�y�DĹ�D���D�9�D�y�DŹ�D���D�9�D�y�Dƹ�D���D�9�D�y�Dǹ�D���D�9�D�y�Dȹ�D���D�9�D�y�Dɹ�D���D�9�D�y�Dʹ�D���D�9�D�y�D˹�D���D�9�D�y�D̹�D���D�9�D�y�D͹�D���D�9�D�y�Dι�D���D�9�D�y�DϹ�D���D�9�D�y�Dй�D���D�9�D�y�Dѹ�D���D�9�D�y�Dҹ�D���D�9�D�y�Dӹ�D���D�9�D�y�DԹ�D���D�9�D�y�Dչ�D���D�9�D�y�Dֹ�D���D�9�D�y�D׹�D���D�9�D�y�Dع�D���D�9�D�y�Dٹ�D���D�9�D�y�Dڹ�D���D�9�D�y�D۹�D���D�9�D�y�Dܹ�D���D�9�D�y�Dݹ�D���D�9�D�y�D޹�D���D�9�D�y�D߹�D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D���D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�=D�y�D��D���D�9�D�y�D��D���D�9�D�}D��D���D�=D�pR11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�;dA�?}A�A�A�A�A�C�A�C�A�C�A�C�A�A�A�C�A�E�A�E�A�G�A�G�A�C�A�C�A�E�A�C�A�E�A�C�A�A�A�A�A�?}A�?}A�A�A�A�A�A�A�=qA�9XA�$�A���A��mA�&�A�&�A�A��uA���A�7LA�ĜA�
=A��A�A�9XA���A��A��A���A�ȴA��;A�bNA�-A��FA�$�A�ƨA�|�A���A�33A���A�ƨA�G�A�%A��DA�M�A�?}A�XA��/A�(�A�ZA�~�A�p�A�VA�\)A�A�A�dZA�ȴA���A�E�A��A�
=A�C�A��7A���A�G�A��/A�O�A���A��yA�|�A�1'A��RA�{A��A��A�JA�A��A�PA�A~��A~=qA}dZA|��A{��Az�AzjAy�FAy+Ax5?Av�9Au��As�
Ar�RAr�Aq\)ApjAo�FAooAn=qAm%Al��AlZAl5?Ak�Ak|�AkAjz�Aj�Ai��Ait�AhM�Ad�uAdbNAd=qAc�Ac��Ac/Ab��Aa%A]�TA\M�AZ9XAX��AXv�AW��AV��AVVAU33AT��AT=qAR�AQ�AO�AN�AM�AL�AK��AK+AJr�AI�TAH�AG�
AEAEoAD��AD-AC�hAC?}AC"�AB��ABffAB-AA��AA�FAAXA@�A>�+A<Q�A;�A;l�A:n�A9�#A9p�A8�+A7XA6z�A5C�A4r�A3�;A2�/A2z�A21'A1��A1oA0�A0r�A0  A/\)A.=qA-O�A,I�A+;dA*v�A)�mA)l�A(��A(�DA'�;A&�A%p�A$�!A$  A#�A"��A"��A"n�A"9XA!��A!"�A   A�;AbNA  A�AƨA�A"�A�AjAZA(�A|�A��AbA��A�7AoA��A�!A�9AVAJAt�A?}A�9AA�A��A�A�wA�
A
�HA	ƨA��A�TA��At�Al�A�AƨA �9@��P@��+@�=q@�Q�@��@�\)@��@��@�M�@�{@��@�G�@��T@�9X@�dZ@��@�V@��`@��@�@�"�@��T@��@�;d@�@���@�9X@��m@��H@�&�@���@�33@�-@��@��@��y@��@�9X@�5?@ְ!@Ԭ@��@�ȴ@�-@̃@�V@�S�@�$�@���@ēu@���@�~�@�5?@���@�7L@��@���@���@�l�@�o@���@��@�X@��@�t�@�J@��/@�r�@�(�@��@���@�ff@��\@��#@��#@���@�V@���@��@�S�@���@��`@��D@�j@��w@�;d@��H@��-@��@���@���@�(�@��@���@�{@���@�p�@�?}@��u@�ƨ@��@���@��@�j@��
@��w@��F@��@��;@�;d@��@���@���@��@���@�9X@��@�K�@�33@��@�ff@�%@�Q�@��w@��!@�J@�?}@���@��w@��!@��\@��@�x�@�/@�z�@�Z@�Q�@��@��@���@�C�@���@�E�@�5?@���@��h@�O�@�7L@���@��D@�(�@��@�b@��@��
@��@���@�l�@�o@�@��@�V@�5?@�J@���@�X@�&�@��@�%@��9@�j@�9X@�  @�ƨ@��P@�|�@�dZ@��y@���@��@��@���@�J@��@�O�@�/@���@���@��@�I�@�1@��
@���@�+@�o@���@��\@�~�@�V@�$�@�`B@��@��u@�I�@�1@��@K�@~�y@~��@}O�@|��@|�/@|�j@|1@z�@z�!@z~�@zJ@yhs@x��@x�@w�@w+@w
=@v��@v�y@v�@v�@v��@u�T@u?}@t��@t�D@s��@s"�@r�@r�!@q��@pĜ@p�9@o�P@m��@l��@l�/@lz�@k�m@k@j��@i��@h�9@g�w@g;d@f��@f�R@f��@f��@f��@f�+@fv�@fv�@fV@fE�@f5?@f$�@e��@e�-@e`B@e?}@d��@c�F@c�@cdZ@b�@b�\@b�@a��@aX@`Ĝ@`r�@`A�@_�;@_�@^�y@^�y@^�@^��@^��@^V@^@]O�@\�@\z�@\�@[��@[�
@[�
@\1@\I�@\9X@\(�@\1@[�
@[S�@[o@Z�@Z��@Z��@Z=q@XĜ@YG�@Yhs@Y7L@Y%@XĜ@XQ�@W�@W�@VV@VV@V@U�h@T��@T�@T�D@T9X@S��@SC�@S@R��@R��@R^5@R=q@R�@RJ@Q��@Q�@P��@O�@O�w@O\)@Nȴ@N�R@N�R@N�R@N�R@N5?@MO�@L�j@Lj@K��@K�
@KdZ@J��@JM�@I�@Ihs@H�u@H��@HQ�@H  @G��@Gl�@G\)@G�@F��@F�@F�y@F�y@G�@G
=@F��@Fv�@F�+@G
=@G
=@F��@F��@Fȴ@F��@F5?@F{@E@E�h@E/@D�@C�m@CC�@B~�@BJ@A��@A�7@Ahs@AX@@�u@@A�@@ �@@  @?��@?l�@?+@>��@>ȴ@>v�@=��@=V@<�D@<�@;�
@;�F@;�@;33@;@:�@:��@9�@9%@8Ĝ@8Ĝ@8Ĝ@8Ĝ@8��@8�u@8�u@8�@8Q�@8 �@7\)@7
=@6�@5�@3�@2�\@2-@2��@2^5@2-@2J@1��@1�#@1��@1��@1�@0r�@0A�@/�w@/|�@/\)@/K�@/+@/
=@.�y@.�R@.E�@-�@-@-?}@-V@,�@,��@,�@,z�@,(�@+�F@+��@+��@+t�@+t�@+t�@+dZ@+dZ@+S�@+"�@*��@*�\@)�@)x�@)�@(��@(�9@(��@(��@( �@'�@'|�@'�@&��@&v�@&@%`B@%?}@%�@$��@$�/@$��@$�j@$�j@$�j@$�@$�@$I�@#�m@$1@#��@#t�@#o@"�!@"�!@"��@"��@"n�@"�@!�#@!��@!��@!��@!�7@!�7@!X@ �`@ ��@ r�@  �@�P@K�@
=@�y@ȴ@�+@V@E�@$�@�@@�-@��@`B@�D@9X@9X@9X@9X@9X@9X@(�@(�@�@1@ƨ@@J@��@��@��@��@��@��@�@�#@��@��@��@�^@��@x�@�7@�7@7L@Ĝ@��@r�@ �@�;@|�@|�@K�@+@�y@�R@��@�+@v�@{@@�-@��@�@p�@O�@/@V@�@��@��@�D@Z@1@ƨ@�@S�@o@�H@��@~�@M�@��@x�@7L@&�@�@%@��@��@��@bN@1'@b@  @�;@��@|�@+@ȴ@�R@�+@ff@V@$�@@�@�T@@@�h@`B@O�@O�@�j@j@I�@��@�
@�
@�F@��@t�@33@o@@
�H@
�\@
=q@
-@	��@	�#@	��@	x�@	hs@	hs@	7L@	%@�`@��@��@�@r�@Q�@1'@b@  @�@  @  @  @  @  @  @��@��@l�@K�@��@�@ȴ@��@v�@v�@V@{@�@��@�-@��@�@?}@/@�@V@��@��@�@�@�/@��@�j@�@�D@�D@�D@z�@9X@�@ƨ@��@C�@�H@~�@^5@n�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�;dA�?}A�A�A�A�A�C�A�C�A�C�A�C�A�A�A�C�A�E�A�E�A�G�A�G�A�C�A�C�A�E�A�C�A�E�A�C�A�A�A�A�A�?}A�?}A�A�A�A�A�A�A�=qA�9XA�$�A���A��mA�&�A�&�A�A��uA���A�7LA�ĜA�
=A��A�A�9XA���A��A��A���A�ȴA��;A�bNA�-A��FA�$�A�ƨA�|�A���A�33A���A�ƨA�G�A�%A��DA�M�A�?}A�XA��/A�(�A�ZA�~�A�p�A�VA�\)A�A�A�dZA�ȴA���A�E�A��A�
=A�C�A��7A���A�G�A��/A�O�A���A��yA�|�A�1'A��RA�{A��A��A�JA�A��A�PA�A~��A~=qA}dZA|��A{��Az�AzjAy�FAy+Ax5?Av�9Au��As�
Ar�RAr�Aq\)ApjAo�FAooAn=qAm%Al��AlZAl5?Ak�Ak|�AkAjz�Aj�Ai��Ait�AhM�Ad�uAdbNAd=qAc�Ac��Ac/Ab��Aa%A]�TA\M�AZ9XAX��AXv�AW��AV��AVVAU33AT��AT=qAR�AQ�AO�AN�AM�AL�AK��AK+AJr�AI�TAH�AG�
AEAEoAD��AD-AC�hAC?}AC"�AB��ABffAB-AA��AA�FAAXA@�A>�+A<Q�A;�A;l�A:n�A9�#A9p�A8�+A7XA6z�A5C�A4r�A3�;A2�/A2z�A21'A1��A1oA0�A0r�A0  A/\)A.=qA-O�A,I�A+;dA*v�A)�mA)l�A(��A(�DA'�;A&�A%p�A$�!A$  A#�A"��A"��A"n�A"9XA!��A!"�A   A�;AbNA  A�AƨA�A"�A�AjAZA(�A|�A��AbA��A�7AoA��A�!A�9AVAJAt�A?}A�9AA�A��A�A�wA�
A
�HA	ƨA��A�TA��At�Al�A�AƨA �9@��P@��+@�=q@�Q�@��@�\)@��@��@�M�@�{@��@�G�@��T@�9X@�dZ@��@�V@��`@��@�@�"�@��T@��@�;d@�@���@�9X@��m@��H@�&�@���@�33@�-@��@��@��y@��@�9X@�5?@ְ!@Ԭ@��@�ȴ@�-@̃@�V@�S�@�$�@���@ēu@���@�~�@�5?@���@�7L@��@���@���@�l�@�o@���@��@�X@��@�t�@�J@��/@�r�@�(�@��@���@�ff@��\@��#@��#@���@�V@���@��@�S�@���@��`@��D@�j@��w@�;d@��H@��-@��@���@���@�(�@��@���@�{@���@�p�@�?}@��u@�ƨ@��@���@��@�j@��
@��w@��F@��@��;@�;d@��@���@���@��@���@�9X@��@�K�@�33@��@�ff@�%@�Q�@��w@��!@�J@�?}@���@��w@��!@��\@��@�x�@�/@�z�@�Z@�Q�@��@��@���@�C�@���@�E�@�5?@���@��h@�O�@�7L@���@��D@�(�@��@�b@��@��
@��@���@�l�@�o@�@��@�V@�5?@�J@���@�X@�&�@��@�%@��9@�j@�9X@�  @�ƨ@��P@�|�@�dZ@��y@���@��@��@���@�J@��@�O�@�/@���@���@��@�I�@�1@��
@���@�+@�o@���@��\@�~�@�V@�$�@�`B@��@��u@�I�@�1@��@K�@~�y@~��@}O�@|��@|�/@|�j@|1@z�@z�!@z~�@zJ@yhs@x��@x�@w�@w+@w
=@v��@v�y@v�@v�@v��@u�T@u?}@t��@t�D@s��@s"�@r�@r�!@q��@pĜ@p�9@o�P@m��@l��@l�/@lz�@k�m@k@j��@i��@h�9@g�w@g;d@f��@f�R@f��@f��@f��@f�+@fv�@fv�@fV@fE�@f5?@f$�@e��@e�-@e`B@e?}@d��@c�F@c�@cdZ@b�@b�\@b�@a��@aX@`Ĝ@`r�@`A�@_�;@_�@^�y@^�y@^�@^��@^��@^V@^@]O�@\�@\z�@\�@[��@[�
@[�
@\1@\I�@\9X@\(�@\1@[�
@[S�@[o@Z�@Z��@Z��@Z=q@XĜ@YG�@Yhs@Y7L@Y%@XĜ@XQ�@W�@W�@VV@VV@V@U�h@T��@T�@T�D@T9X@S��@SC�@S@R��@R��@R^5@R=q@R�@RJ@Q��@Q�@P��@O�@O�w@O\)@Nȴ@N�R@N�R@N�R@N�R@N5?@MO�@L�j@Lj@K��@K�
@KdZ@J��@JM�@I�@Ihs@H�u@H��@HQ�@H  @G��@Gl�@G\)@G�@F��@F�@F�y@F�y@G�@G
=@F��@Fv�@F�+@G
=@G
=@F��@F��@Fȴ@F��@F5?@F{@E@E�h@E/@D�@C�m@CC�@B~�@BJ@A��@A�7@Ahs@AX@@�u@@A�@@ �@@  @?��@?l�@?+@>��@>ȴ@>v�@=��@=V@<�D@<�@;�
@;�F@;�@;33@;@:�@:��@9�@9%@8Ĝ@8Ĝ@8Ĝ@8Ĝ@8��@8�u@8�u@8�@8Q�@8 �@7\)@7
=@6�@5�@3�@2�\@2-@2��@2^5@2-@2J@1��@1�#@1��@1��@1�@0r�@0A�@/�w@/|�@/\)@/K�@/+@/
=@.�y@.�R@.E�@-�@-@-?}@-V@,�@,��@,�@,z�@,(�@+�F@+��@+��@+t�@+t�@+t�@+dZ@+dZ@+S�@+"�@*��@*�\@)�@)x�@)�@(��@(�9@(��@(��@( �@'�@'|�@'�@&��@&v�@&@%`B@%?}@%�@$��@$�/@$��@$�j@$�j@$�j@$�@$�@$I�@#�m@$1@#��@#t�@#o@"�!@"�!@"��@"��@"n�@"�@!�#@!��@!��@!��@!�7@!�7@!X@ �`@ ��@ r�@  �@�P@K�@
=@�y@ȴ@�+@V@E�@$�@�@@�-@��@`B@�D@9X@9X@9X@9X@9X@9X@(�@(�@�@1@ƨ@@J@��@��@��@��@��@��@�@�#@��@��@��@�^@��@x�@�7@�7@7L@Ĝ@��@r�@ �@�;@|�@|�@K�@+@�y@�R@��@�+@v�@{@@�-@��@�@p�@O�@/@V@�@��@��@�D@Z@1@ƨ@�@S�@o@�H@��@~�@M�@��@x�@7L@&�@�@%@��@��@��@bN@1'@b@  @�;@��@|�@+@ȴ@�R@�+@ff@V@$�@@�@�T@@@�h@`B@O�@O�@�j@j@I�@��@�
@�
@�F@��@t�@33@o@@
�H@
�\@
=q@
-@	��@	�#@	��@	x�@	hs@	hs@	7L@	%@�`@��@��@�@r�@Q�@1'@b@  @�@  @  @  @  @  @  @��@��@l�@K�@��@�@ȴ@��@v�@v�@V@{@�@��@�-@��@�@?}@/@�@V@��@��@�@�@�/@��@�j@�@�D@�D@�D@z�@9X@�@ƨ@��@C�@�H@~�@^5@n�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BɺBȴBȴBȴBȴBɺBɺBɺBɺBɺBɺBɺBɺBȴBȴBɺBȴBȴBȴBȴBȴBȴBȴBȴBȴBȴBƨBĜB�qB��Bm�B�uBu�B�B�3B�LB�dB�^B�3B��B��B��B��B�{B�PBx�BjBVB;dB&�B{B��B�B�
B��B�?B�#B�B��B��B��B�9B��B�JB�1B�Bo�BgmBXBQ�BP�BB�B7LB1'B#�B\BJB%B
��B
��B
�B
�fB
�HB
�B
��B
ȴB
��B
�qB
�3B
��B
��B
��B
��B
��B
��B
��B
�hB
�VB
�DB
�B
�B
{�B
s�B
r�B
l�B
hsB
aHB
VB
M�B
C�B
:^B
8RB
2-B
/B
)�B
&�B
!�B
�B
�B
�B
�B
�B
�B
oB
\B
PB

=B
%B	��B	�`B	�B	�B	�sB	�ZB	�BB	�B	ɺB	�FB	�'B	��B	��B	��B	��B	��B	��B	�hB	�\B	�=B	� B	v�B	r�B	l�B	bNB	\)B	S�B	S�B	M�B	J�B	C�B	<jB	2-B	8RB	8RB	5?B	33B	2-B	2-B	0!B	,B	+B	(�B	$�B	�B	�B	
=B��B	+B	B��B��B��B�B�B�yB�`B�TB�ZB�HB�HB�NB�;B�/B�)B�)B�B��B��BɺBƨBB��B�}B�qB�dB�LB�-B�B��B��B��B��B��B��B��B��B��B��B�PB�B�%B�PB�bB�VB�JB�=B�=B�1B�7B�+B�B~�B}�B~�B{�Bv�Bo�BgmB`BBdZBe`B`BBe`BaHB_;B[#BW
BO�BK�BJ�BG�B@�B2-B1'BC�BB�B<jB49B49B49B8RB9XB33B5?B5?B,B�B+B49B2-B-B#�B+B/B-B0!B33B1'B.B.B+B)�B+B(�B+B+B+B'�B%�B-B(�B%�B-B/B;dB:^B6FB/B6FB/B#�B"�B$�B"�B�B�B!�B#�B&�B$�B!�B&�B'�B%�B%�B(�B,B,B-B.B0!B2-B1'B0!B1'B0!B49B5?B5?B5?B9XB<jB>wBB�BG�BE�BK�BI�BE�BB�BD�BF�BG�BF�BF�BG�BC�BA�BD�BI�BP�BQ�BR�BT�BXB\)B\)BZBZB]/B_;BcTBhsBjBo�Bq�Bt�Bv�Bw�B|�B~�B}�B{�By�B}�B�B�%B�+B�%B�B�B�1B�=B�PB��B��B��B��B��B��B��B�B�B�B�9B�?B�9B�?B�FB�LB�RB�jB�}B�qB��BÖBŢBŢBȴB��B��B��B��B��B��B��B��B��B�B�B�B�#B�#B�#B�#B�;B�NB�`B�fB�fB�mB�yB�B�B�B�B�B�B�B�B�B�B��B��B��B��B	B	B	B	B	1B	DB	VB	bB	hB	�B	�B	�B	�B	�B	!�B	%�B	)�B	,B	.B	0!B	1'B	2-B	2-B	;dB	>wB	>wB	=qB	=qB	A�B	B�B	B�B	C�B	F�B	J�B	L�B	O�B	T�B	VB	VB	VB	VB	W
B	XB	[#B	_;B	_;B	`BB	dZB	e`B	e`B	ffB	gmB	k�B	o�B	q�B	w�B	x�B	x�B	z�B	{�B	� B	�B	�B	�%B	�JB	�VB	�VB	�\B	�bB	�bB	�bB	�hB	�hB	�hB	�hB	�hB	�oB	�hB	�oB	�oB	�oB	�hB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�-B	�3B	�?B	�LB	�RB	�XB	�dB	��B	ĜB	ǮB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�
B	�B	�/B	�5B	�5B	�BB	�TB	�ZB	�ZB	�`B	�`B	�mB	�mB	�yB	�yB	�yB	�yB	�B	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
%B
%B
%B
+B
	7B
	7B

=B
DB
JB
JB
JB
JB
JB
VB
\B
bB
bB
hB
hB
\B
hB
oB
oB
uB
uB
uB
{B
{B
{B
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
 �B
"�B
#�B
$�B
$�B
$�B
$�B
$�B
#�B
"�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
$�B
#�B
#�B
$�B
%�B
%�B
&�B
'�B
'�B
'�B
'�B
'�B
(�B
+B
,B
,B
-B
.B
.B
.B
.B
.B
-B
/B
.B
/B
/B
0!B
1'B
2-B
33B
49B
49B
5?B
49B
49B
5?B
6FB
6FB
8RB
8RB
8RB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
:^B
:^B
>wB
=qB
=qB
<jB
=qB
>wB
>wB
>wB
=qB
=qB
=qB
>wB
@�B
@�B
A�B
A�B
A�B
@�B
@�B
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
C�B
C�B
C�B
B�B
C�B
D�B
E�B
E�B
E�B
E�B
D�B
E�B
E�B
D�B
D�B
C�B
C�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
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
M�B
M�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
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
S�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
VB
VB
VB
VB
W
B
XB
XB
XB
XB
XB
XB
XB
YB
YB
YB
YB
YB
YB
YB
YB
[#B
ZB
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
\)B
\)B
\)B
^5B
]/B
^5B
_;B
^5B
_;B
_;B
_;B
`BB
`BB
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
cTB
cTB
cTB
cTB
dZB
cTB
dZB
dZB
dZB
e`B
e`B
dZB
dZB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
ffB
ffB
ffB
ffB
gmB
ffB
ffB
gmB
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
iyB
iyB
iyB
iyB
iyB
jB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
jB
jB
l�B
l�B
l�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B��B��B��B��B��BɺBɺB��B��BɺBɺB��B��B��B��BɺB��BȴB��B��B��BȴBȴB��B��B��B��B�SB��B��By�B�CB��B��B�B�$B��B��B�tB�QB�&B�0B�@B�_B��B}qBmCBZB>�B*B�B�B�BܒB�EB��B�]B�7B�,B��B̘B��B�sB�.B��B�zBs�Bi�BZ�BT�BSuBEB9rB2�B&�B�B�B�B
��B
��B
�B
�
B
�B
ۦB
өB
�XB
��B
�wB
��B
�B
��B
�yB
��B
��B
��B
�B
� B
��B
��B
�?B
��B
}<B
uB
shB
m�B
iyB
b�B
W�B
OvB
E�B
;�B
9>B
3MB
0UB
+B
'�B
#B
B
CB
B
	B
B
B
&B
.B
�B

�B
B	�B	��B	��B	��B	��B	�B	�B	�KB	�JB	��B	�hB	�KB	��B	��B	��B	��B	��B	��B	�.B	�xB	�'B	x�B	tTB	n/B	c�B	]�B	UMB	T�B	N�B	K�B	EB	>(B	4nB	9>B	9	B	6B	4B	2�B	2|B	0�B	,�B	+�B	)yB	%zB	 �B	$B	�B	oB	�B	B�.B��B��B�B�B��B�B�B�`B�B��B��B�'B�B��BܬB��B�B�}B�B�B�B��B�iB�(B�6B�B�hB�qB��B�
B�B��B�`B�LB�@B�bB��B��B�BB��B��B��B��B��B��B��B��B��B��B��B�'B� B~�B�B|�Bw�Bq[BiyBb�BeBe�Ba-Be�Bb4B`'B\CBX�BQ�BN<BLdBI�BB�B5�B3�BC�BCB=�B6B5�B5�B9$B9�B4�B6B6B.B!�B,"B4nB2�B./B&B,=B/�B./B0�B3�B1�B.�B.�B,"B+B,"B*B+�B+�B+�B(�B'B-�B)�B&�B-]B/OB;0B;JB7�B0oB6�B0�B&LB$ZB%�B$ZB�B!B"�B$�B'mB%�B"�B'RB(XB&�B&�B)yB,WB,�B-�B.�B0�B2�B1�B1'B2GB1B4�B5�B5�B5�B9�B<�B>�BB�BG�BFtBL0BJXBF�BC�BE9BGBG�BGEBGEBHKBD�BC-BE�BJ�BQ�BR�BS�BU�BX�B\xB\�BZ�BZ�B^B`'BdBiBkBo�Bq�Bt�Bw2BxlB}VB.B~]B|�Bz�B~�B��B�tB�zB�tB��B�'B�B��B�<B�B�EB�CB��B��B�$B��B��B�}B��B�nB�tB��B�tB��B��B��B��B��B��B��B��B��B�%B�B�)B�B�B�(B�B�.B�B�4B�NB�9B�_BؓB�WB�qBیBۦBߊB�B�B��B��B�B��B��B��B��B��B��B��B��B��B��B�GB�LB�"B�(B�HB	;B	UB	uB	mB	�B	�B	�B	�B	�B	�B	�B	�B		B	WB	"B	&LB	*KB	,WB	.cB	0oB	1vB	2|B	2�B	;B	>�B	>�B	=�B	>B	A�B	B�B	B�B	C�B	F�B	KB	MB	PHB	T�B	V9B	V9B	V9B	V9B	WYB	XyB	[qB	_pB	_�B	`�B	d�B	e�B	e�B	f�B	g�B	k�B	p;B	raB	xB	y	B	y$B	{JB	|jB	�OB	�oB	��B	��B	�~B	�pB	��B	��B	�}B	�bB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�-B	��B	�B	�B	�B	�B	�8B	�XB	�XB	�QB	�]B	�5B	�[B	�aB	�MB	�ZB	�LB	�lB	��B	��B	��B	�B	��B	��B	��B	�	B	�B	�DB	��B	�B	�2B	�2B	�9B	�SB	�YB	�sB	�QB	�IB	�jB	ބB	��B	�B	�B	�B	�B	��B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�B	�B	�B	��B	�B	�B	��B	��B	�B	�B	�B	�B	��B	�6B	�"B	�"B	�"B	�"B	�(B	�B
 B
 B
 B
;B
AB
GB
[B
B
B
?B
YB
YB
EB
	lB
	lB

rB
xB
~B
�B
�B
�B
�B
�B
�B
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
B
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
B
�B
�B
EB
+B
	B
�B
 �B
#B
$B
%B
%B
$�B
%B
%B
$@B
# B
$B
$&B
$&B
%B
%B
%B
$�B
%B
$B
$&B
$�B
&B
&B
'B
($B
($B
($B
($B
($B
)DB
+6B
,"B
,=B
-)B
./B
./B
.B
.IB
./B
-]B
/OB
.IB
/iB
/OB
0UB
1[B
2aB
3�B
4�B
4�B
5tB
4�B
4nB
5tB
6�B
6�B
8lB
8lB
8�B
9�B
9rB
9rB
9rB
9rB
9rB
9rB
:�B
:�B
>�B
=�B
=�B
<�B
=�B
>�B
>wB
>�B
=�B
=�B
=�B
>�B
@�B
@�B
A�B
A�B
A�B
@�B
@�B
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
C�B
C�B
C�B
B�B
C�B
D�B
E�B
E�B
E�B
E�B
D�B
E�B
E�B
D�B
D�B
DB
C�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
J�B
J�B
J�B
KB
K�B
K�B
K�B
K�B
K�B
M�B
NB
OB
OB
O�B
O�B
O�B
PB
PB
QB
RB
SB
S&B
SB
SB
S&B
S&B
S&B
S@B
TB
TFB
T,B
T,B
U2B
U2B
UB
UB
U2B
U2B
V9B
V9B
V9B
VSB
W$B
X+B
X+B
X+B
XEB
XEB
XEB
X_B
YKB
YKB
Y1B
YKB
YKB
Y1B
YKB
YKB
[WB
ZQB
[=B
[WB
[qB
[WB
\)B
\CB
\]B
\CB
\]B
\]B
\CB
\]B
\xB
\]B
^jB
]dB
^jB
_VB
^jB
_VB
_pB
_pB
`vB
`vB
`vB
`\B
`vB
abB
a|B
a|B
a|B
a|B
bhB
b�B
b�B
b�B
b�B
c�B
c�B
c�B
c�B
dtB
c�B
d�B
dtB
dtB
ezB
ezB
dtB
dtB
dtB
dtB
dtB
dtB
dtB
e�B
e�B
f�B
f�B
f�B
f�B
g�B
f�B
f�B
g�B
g�B
g�B
g�B
g�B
g�B
hsB
h�B
iyB
i�B
i�B
i�B
i�B
i�B
i�B
iyB
i�B
i�B
j�B
i�B
i�B
i�B
i�B
i�B
i�B
i�B
j�B
j�B
l�B
l�B
l�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111131111111111111111111131111111111111131111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<B��<#�
<>�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.19(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201710230036172017102300361720171023003617201806221232182018062212321820180622123218201804050427542018040504275420180405042754  JA  ARFMdecpA19c                                                                20171019093508  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20171019003513  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20171019003515  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20171019003516  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20171019003516  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20171019003516  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20171019003517  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20171019003517  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20171019003517  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20171019003518                      G�O�G�O�G�O�                JA  ARUP                                                                        20171019005656                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20171019153301  CV  JULD            G�O�G�O�F�u�                JM  ARSQJMQC2.0                                                                 20171020000000  CF  PSAL_ADJUSTED_QCD�@ D�@ G�O�                JM  ARCAJMQC2.0                                                                 20171022153617  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20171022153617  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404192754  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622033218  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115121517                      G�O�G�O�G�O�                