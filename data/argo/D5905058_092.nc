CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-09-27T18:35:22Z creation;2018-09-27T18:35:24Z conversion to V3.1;2019-12-23T06:14:26Z update;     
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
resolution        =���   axis      Z        H  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     H  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \    TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     H  _�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     H  r�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �8   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     H  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �T   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     H  �(   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �p   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     H  �D   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     H  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     H  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  `  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �|   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �|   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �|   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  �|   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
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
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20180927183522  20200120021520  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               \A   JA  I2_0675_092                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @؄���1   @؄��`�@73�����cQ��,=1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  BffB  B   B(  B0  B8  B@  BH  BPffBX  B_��Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
fD
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D%��D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\fD\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D��3D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ D�|�D�� D�  D�@ DԀ D�� D�3D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�=q@��
A	�A)�AI�Ai�A���A���A���A���A���A���A���A���Bz�B
z�B�HBz�B"z�B*z�B2z�B:z�BBz�BJz�BR�HBZz�Bb{Bjz�Brz�Bzz�B�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qC ��C��C��C��C��C
��C��C��C��C��C��C��C��C��C��C��C ��C"��C$��C&��C(��C*��C,��C.��C0��C2��C4��C6��C8��C:��C<��C>��C@��CB��CD��CF��CH��CJ��CL��CN��CP��CR��CT��CV��CX��CZ��C\��C^��C`��Cb��Cd��Cf��Ch��Cj��Cl��Cn��Cp��Cr��Ct��Cv��Cx��Cz��C|��C~��C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�B�C�O\D '�D ��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D	'�D	��D
.D
��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D '�D ��D!'�D!��D"'�D"��D#'�D#��D$'�D$��D%'�D%��D&!HD&��D''�D'��D('�D(��D)'�D)��D*'�D*��D+'�D+��D,'�D,��D-'�D-��D.'�D.��D/'�D/��D0'�D0��D1'�D1��D2'�D2��D3'�D3��D4'�D4��D5'�D5��D6'�D6��D7'�D7��D8'�D8��D9'�D9��D:'�D:��D;'�D;��D<'�D<��D='�D=��D>'�D>��D?'�D?��D@'�D@��DA'�DA��DB'�DB��DC'�DC��DD'�DD��DE'�DE��DF'�DF��DG'�DG��DH'�DH��DI'�DI��DJ'�DJ��DK'�DK��DL'�DL��DM'�DM��DN'�DN��DO'�DO��DP'�DP��DQ'�DQ��DR'�DR��DS'�DS��DT'�DT��DU'�DU��DV'�DV��DW'�DW��DX'�DX��DY'�DY��DZ'�DZ��D['�D[��D\.D\��D]'�D]��D^'�D^��D_'�D_��D`'�D`��Da'�Da��Db'�Db��Dc'�Dc��Dd'�Dd��De'�De��Df'�Df��Dg'�Dg��Dh'�Dh��Di'�Di��Dj'�Dj��Dk'�Dk��Dl'�Dl��Dm'�Dm��Dn'�Dn��Do'�Do��Dp'�Dp��Dq'�Dq��Dr'�Dr��Ds'�Ds��Dt'�Dt��Du'�Du��Dv'�Dv��Dw'�Dw��Dx'�Dx��Dy'�Dy��Dz'�Dz��D{'�D{��D|'�D|��D}'�D}��D~'�D~��D'�D��D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D�D��
D��D�S�DÓ�D���D��D�S�Dē�D���D��D�S�Dœ�D���D��D�S�DƓ�D���D��D�S�DǓ�D���D��D�S�Dȓ�D���D��D�S�Dɓ�D���D��D�S�Dʓ�D���D��D�S�D˓�D���D��D�S�D̓�D���D��D�S�D͓�D���D��D�S�DΓ�D���D��D�S�Dϓ�D���D��D�S�DГ�D���D��D�S�Dѓ�D���D��D�S�Dғ�D���D��D�S�DӐ�D���D��D�S�Dԓ�D���D�
D�S�DՓ�D���D��D�S�D֓�D���D��D�S�Dד�D���D��D�S�Dؓ�D���D��D�S�Dٓ�D���D��D�S�Dړ�D���D��D�S�Dۓ�D���D��D�S�Dܓ�D���D��D�S�Dݓ�D���D��D�S�Dޓ�D���D��D�S�Dߓ�D���D��D�S�D���D���D��D�S�D��D���D��D�S�D��D���D��D�S�D��D���D��D�S�D��D���D��D�S�D��D���D��D�S�D��D���D��D�S�D��D���D��D�S�D��D���D��D�S�D��D���D��D�S�D��D���D��D�S�D��D���D��D�S�D��D���D��D�S�D��D���D��D�S�D��D���D��D�S�D��D���D��D�S�D��D���D��D�S�D��D���D��D�S�D��D���D��D�S�D��D���D��D�S�D��
D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A��A���A��TA�ȴAЋDA�C�A�A���A��A��mA��TA��/A��A���A���A���A���A���A���A�ȴA�ƨA�ĜAϺ^Aϥ�AϋDA��
AŴ9A�M�A�$�A�5?A���A��HA���A��DA���A�\)A�ĜA���A�%A�~�A��HA�t�A�;dA�x�A���A�M�A��
A�{A��A��HA�ZA�Q�A���A��uA�VA���A�\)A�%A���A�?}A�%A��A�E�A�ffA���A�bNA��/A�E�A�t�A��9A�9XA��hA���A��#A�5?A�?}A�z�A�C�A��-A�A�(�A���A�I�A���A�bA�  A�(�A�v�A��jA���A��A���A�v�A���A��9A�7LA�ƨA��A�XA�-A��A�O�A���A���A��7A}�wA{�#Az�RAz$�Ay�;Ay�PAy
=Ax��Ax��AxZAwl�At�+As�Aq��An��Al�`Ak��Ak�Aj  Ah�9Afr�Acp�Aa�wA`�A^(�A\�`A[�mAZ��AW�-AW
=AVZAU�hAUG�AS��AR��AQ��AO�AN�yALVAKdZAJjAI�AH�/AH{AGK�ADQ�AA�AA�A@1'A?/A=dZA;��A:9XA9oA6�A5�A4��A3dZA2Q�A1ƨA/�A-��A,bNA*��A*��A*r�A)`BA)VA(n�A'l�A&�\A%��A%�A%A$�A$��A${A#�PA"��A!��A!&�A �DA�;A"�A~�A�/Av�AI�A��A��A�DAC�A��A(�A;dA�jA��AjA��A��A+A
=A��A�AoA�
AVA
��A
��A
^5A	�TA	7LA��AA�`A{A��A�`Ax�AG�A"�AffA&�A Z@��@��@�"�@�/@���@���@�V@�t�@�^5@�@�@��m@�x�@�|�@���@�u@���@��;@ܓu@�o@���@١�@�V@�@���@�p�@Ԭ@�"�@�hs@���@�l�@�{@�?}@̓u@�1'@˶F@�C�@���@���@Ǿw@Ə\@���@���@�r�@�A�@��@�33@�n�@�J@��@�Z@��w@�l�@�K�@���@��@��@�p�@�r�@��m@�
=@�v�@��^@���@���@���@��@�S�@���@���@��@���@�Z@��;@�@���@�~�@�v�@�J@���@���@���@�`B@��9@�Z@���@��w@�l�@�
=@���@�n�@��@��@���@�K�@���@���@���@�?}@��@��/@��@��/@�r�@�  @�dZ@�+@��H@�@��@���@���@�9X@�A�@��/@��`@�`B@�`B@��@���@��9@��@���@�z�@�I�@�(�@���@�@�~�@���@��@�+@�S�@�C�@��@��y@��R@�5?@��-@��@�`B@�G�@�%@�z�@��@��@�@�O�@��@��@��@��9@��@�%@���@��@�j@�(�@� �@�9X@�A�@�Z@�Z@�Z@�bN@�b@�1@��F@�
=@�ȴ@�v�@��@��#@�/@���@���@��`@��@��/@��j@�j@��@��m@�ƨ@��w@�dZ@��@��@���@�n�@�^5@��@���@�`B@�7L@��@�Ĝ@��u@�bN@��@��;@�ƨ@��@���@��P@�\)@�"�@��@���@��+@�V@�$�@�{@��@���@�p�@�/@��@��9@�Q�@�(�@��
@��w@���@�t�@�C�@�@��@���@�=q@�@��@���@���@�&�@�V@�%@��`@��u@�  @��@�"�@��H@�ȴ@��R@���@��+@�v�@�^5@�-@��@���@���@��@��@�r�@�Q�@�Q�@�1'@��;@��@�l�@�C�@��@���@��+@�~�@�~�@�~�@�@��7@�X@�X@�7L@��j@��u@�r�@�Q�@�I�@�1'@�b@�@K�@~�y@~ȴ@~��@~E�@~$�@~@}�T@}@}�h@}`B@}�@|�@|(�@{��@{C�@z��@z~�@y�#@y�7@xQ�@w�@vv�@vE�@v@u��@up�@u?}@t�@tZ@t�@t1@sƨ@sƨ@s�F@sdZ@r��@q�@q�#@q�#@qX@q�@p��@pbN@pb@o�@o;d@o
=@n�@nff@n$�@n@m��@m�h@mO�@m�@l��@lZ@l9X@k�
@k�@kt�@ko@j�@i7L@hĜ@hQ�@hQ�@g�@g|�@g\)@g
=@fff@f5?@f5?@f@e��@e�@cƨ@b�H@b�H@b��@b=q@a��@aG�@`�@_�@_�@_;d@^�y@^5?@]�h@\�@\�@[t�@Z�!@Z��@Z��@Z�\@Z^5@Y�#@YX@YG�@Y&�@XĜ@XQ�@W�;@W�w@W�P@W;d@V�y@V{@U�h@UV@T�D@T�@S�@So@R�H@R��@RM�@R�@Q��@Q�7@Q7L@P�9@PQ�@P �@P  @O�@O�@Nȴ@N�+@N�+@Nv�@N5?@M�@M/@L�@Lz�@LZ@L(�@K��@KS�@J��@J�\@J=q@JJ@I�#@I��@I7L@H��@HĜ@H�@H  @G��@G\)@G+@Fv�@F$�@E�@E��@E/@EV@D�@D�j@D�@D�D@Dz�@Dz�@Dj@DI�@D9X@D(�@C�
@Co@BM�@A��@A�@A��@AX@A7L@@��@@�u@@b@?��@?|�@?\)@?;d@?+@?
=@?
=@?�@?
=@>��@>��@>V@=�@=��@=��@=p�@=?}@<��@<��@;�m@;��@;dZ@;S�@;"�@:��@:��@:=q@9��@9��@9x�@9&�@8Ĝ@8��@8�u@8bN@8bN@81'@8  @7��@7��@7+@6V@6E�@65?@6{@5�@5��@5��@5�@5`B@5?}@4�@4�@4�D@4j@4I�@41@3ƨ@3��@3�@3t�@3S�@3o@2��@2��@2^5@2�@1hs@1�@1�@1�@0��@0�@0  @/�w@/��@/��@/|�@/;d@/;d@/�@/
=@.��@.�R@.ff@.V@.$�@.@-��@-�@-�@,�@,��@,Z@,(�@+�m@+�
@+�F@+�F@+��@+dZ@+"�@*�H@*�\@*�@*J@)��@)�7@)hs@)%@(�@(A�@(  @'�w@'��@'�P@'�P@'�P@'l�@'l�@'\)@'K�@';d@'
=@&��@&��@&��@&��@&{@%��@%O�@%�@%�@$��@$��@$��@$j@$I�@$I�@$I�@$(�@$1@#t�@"�H@"�!@"M�@!x�@!X@!7L@!&�@!%@ �`@ �9@ bN@  �@�;@��@K�@�@ȴ@��@�+@ff@E�@@��@@��@`B@`B@O�@/@/@V@��@(�@�
@ƨ@ƨ@�F@��@��@t�@t�@S�@"�@@�@��@��@�\@~�@n�@=q@-@-@��@�#@�^@�7@X@7L@&�@%@��@��@�`@��@��@�u@�u@�@r�@r�@r�@r�@Q�@ �@  @��@l�@K�@K�@K�@�@�+@V@5?@�T@��@p�@`B@?}@V@�/@�@��@�D@z�@1@�
@��@�@�@t�@"�@�@�!@~�@^5@=q@��@�#@�#@��@�7@&�@�`@�u@bN@1'@  @��@�w@�P@K�@
=@ȴ@��@v�@5?@$�@��@�@p�@p�@p�@?}@�/@z�@1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A��A���A��TA�ȴAЋDA�C�A�A���A��A��mA��TA��/A��A���A���A���A���A���A���A�ȴA�ƨA�ĜAϺ^Aϥ�AϋDA��
AŴ9A�M�A�$�A�5?A���A��HA���A��DA���A�\)A�ĜA���A�%A�~�A��HA�t�A�;dA�x�A���A�M�A��
A�{A��A��HA�ZA�Q�A���A��uA�VA���A�\)A�%A���A�?}A�%A��A�E�A�ffA���A�bNA��/A�E�A�t�A��9A�9XA��hA���A��#A�5?A�?}A�z�A�C�A��-A�A�(�A���A�I�A���A�bA�  A�(�A�v�A��jA���A��A���A�v�A���A��9A�7LA�ƨA��A�XA�-A��A�O�A���A���A��7A}�wA{�#Az�RAz$�Ay�;Ay�PAy
=Ax��Ax��AxZAwl�At�+As�Aq��An��Al�`Ak��Ak�Aj  Ah�9Afr�Acp�Aa�wA`�A^(�A\�`A[�mAZ��AW�-AW
=AVZAU�hAUG�AS��AR��AQ��AO�AN�yALVAKdZAJjAI�AH�/AH{AGK�ADQ�AA�AA�A@1'A?/A=dZA;��A:9XA9oA6�A5�A4��A3dZA2Q�A1ƨA/�A-��A,bNA*��A*��A*r�A)`BA)VA(n�A'l�A&�\A%��A%�A%A$�A$��A${A#�PA"��A!��A!&�A �DA�;A"�A~�A�/Av�AI�A��A��A�DAC�A��A(�A;dA�jA��AjA��A��A+A
=A��A�AoA�
AVA
��A
��A
^5A	�TA	7LA��AA�`A{A��A�`Ax�AG�A"�AffA&�A Z@��@��@�"�@�/@���@���@�V@�t�@�^5@�@�@��m@�x�@�|�@���@�u@���@��;@ܓu@�o@���@١�@�V@�@���@�p�@Ԭ@�"�@�hs@���@�l�@�{@�?}@̓u@�1'@˶F@�C�@���@���@Ǿw@Ə\@���@���@�r�@�A�@��@�33@�n�@�J@��@�Z@��w@�l�@�K�@���@��@��@�p�@�r�@��m@�
=@�v�@��^@���@���@���@��@�S�@���@���@��@���@�Z@��;@�@���@�~�@�v�@�J@���@���@���@�`B@��9@�Z@���@��w@�l�@�
=@���@�n�@��@��@���@�K�@���@���@���@�?}@��@��/@��@��/@�r�@�  @�dZ@�+@��H@�@��@���@���@�9X@�A�@��/@��`@�`B@�`B@��@���@��9@��@���@�z�@�I�@�(�@���@�@�~�@���@��@�+@�S�@�C�@��@��y@��R@�5?@��-@��@�`B@�G�@�%@�z�@��@��@�@�O�@��@��@��@��9@��@�%@���@��@�j@�(�@� �@�9X@�A�@�Z@�Z@�Z@�bN@�b@�1@��F@�
=@�ȴ@�v�@��@��#@�/@���@���@��`@��@��/@��j@�j@��@��m@�ƨ@��w@�dZ@��@��@���@�n�@�^5@��@���@�`B@�7L@��@�Ĝ@��u@�bN@��@��;@�ƨ@��@���@��P@�\)@�"�@��@���@��+@�V@�$�@�{@��@���@�p�@�/@��@��9@�Q�@�(�@��
@��w@���@�t�@�C�@�@��@���@�=q@�@��@���@���@�&�@�V@�%@��`@��u@�  @��@�"�@��H@�ȴ@��R@���@��+@�v�@�^5@�-@��@���@���@��@��@�r�@�Q�@�Q�@�1'@��;@��@�l�@�C�@��@���@��+@�~�@�~�@�~�@�@��7@�X@�X@�7L@��j@��u@�r�@�Q�@�I�@�1'@�b@�@K�@~�y@~ȴ@~��@~E�@~$�@~@}�T@}@}�h@}`B@}�@|�@|(�@{��@{C�@z��@z~�@y�#@y�7@xQ�@w�@vv�@vE�@v@u��@up�@u?}@t�@tZ@t�@t1@sƨ@sƨ@s�F@sdZ@r��@q�@q�#@q�#@qX@q�@p��@pbN@pb@o�@o;d@o
=@n�@nff@n$�@n@m��@m�h@mO�@m�@l��@lZ@l9X@k�
@k�@kt�@ko@j�@i7L@hĜ@hQ�@hQ�@g�@g|�@g\)@g
=@fff@f5?@f5?@f@e��@e�@cƨ@b�H@b�H@b��@b=q@a��@aG�@`�@_�@_�@_;d@^�y@^5?@]�h@\�@\�@[t�@Z�!@Z��@Z��@Z�\@Z^5@Y�#@YX@YG�@Y&�@XĜ@XQ�@W�;@W�w@W�P@W;d@V�y@V{@U�h@UV@T�D@T�@S�@So@R�H@R��@RM�@R�@Q��@Q�7@Q7L@P�9@PQ�@P �@P  @O�@O�@Nȴ@N�+@N�+@Nv�@N5?@M�@M/@L�@Lz�@LZ@L(�@K��@KS�@J��@J�\@J=q@JJ@I�#@I��@I7L@H��@HĜ@H�@H  @G��@G\)@G+@Fv�@F$�@E�@E��@E/@EV@D�@D�j@D�@D�D@Dz�@Dz�@Dj@DI�@D9X@D(�@C�
@Co@BM�@A��@A�@A��@AX@A7L@@��@@�u@@b@?��@?|�@?\)@?;d@?+@?
=@?
=@?�@?
=@>��@>��@>V@=�@=��@=��@=p�@=?}@<��@<��@;�m@;��@;dZ@;S�@;"�@:��@:��@:=q@9��@9��@9x�@9&�@8Ĝ@8��@8�u@8bN@8bN@81'@8  @7��@7��@7+@6V@6E�@65?@6{@5�@5��@5��@5�@5`B@5?}@4�@4�@4�D@4j@4I�@41@3ƨ@3��@3�@3t�@3S�@3o@2��@2��@2^5@2�@1hs@1�@1�@1�@0��@0�@0  @/�w@/��@/��@/|�@/;d@/;d@/�@/
=@.��@.�R@.ff@.V@.$�@.@-��@-�@-�@,�@,��@,Z@,(�@+�m@+�
@+�F@+�F@+��@+dZ@+"�@*�H@*�\@*�@*J@)��@)�7@)hs@)%@(�@(A�@(  @'�w@'��@'�P@'�P@'�P@'l�@'l�@'\)@'K�@';d@'
=@&��@&��@&��@&��@&{@%��@%O�@%�@%�@$��@$��@$��@$j@$I�@$I�@$I�@$(�@$1@#t�@"�H@"�!@"M�@!x�@!X@!7L@!&�@!%@ �`@ �9@ bN@  �@�;@��@K�@�@ȴ@��@�+@ff@E�@@��@@��@`B@`B@O�@/@/@V@��@(�@�
@ƨ@ƨ@�F@��@��@t�@t�@S�@"�@@�@��@��@�\@~�@n�@=q@-@-@��@�#@�^@�7@X@7L@&�@%@��@��@�`@��@��@�u@�u@�@r�@r�@r�@r�@Q�@ �@  @��@l�@K�@K�@K�@�@�+@V@5?@�T@��@p�@`B@?}@V@�/@�@��@�D@z�@1@�
@��@�@�@t�@"�@�@�!@~�@^5@=q@��@�#@�#@��@�7@&�@�`@�u@bN@1'@  @��@�w@�P@K�@
=@ȴ@��@v�@5?@$�@��@�@p�@p�@p�@?}@�/@z�@1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B$�B%�B(�B0!B2-B49B49B49B49B49B49B49B49B49B49B49B49B49B49B49B49B49B49B49B33B/B+B?}BP�BYBYB\)Be`BhsBo�Br�Bt�Bt�Br�Bu�Bw�B{�B|�B{�B|�By�Bx�B{�B~�B~�B}�B~�Bz�Bu�Bl�BhsBl�Br�Bu�By�B|�B}�B~�B� Bu�BffBe`B]/BW
BK�BB�B>wB:^B2-B.B�B{B	7B��B�B�#B��B�3B��B�{B�%BjB;dB1B
�fB
ĜB
�3B
�{B
�=B
�B
� B
y�B
r�B
o�B
l�B
jB
ffB
^5B
S�B
J�B
E�B
?}B
2-B
,B
'�B
%�B
#�B
�B
�B
�B
�B
oB
B	�B	�yB	�
B	ǮB	��B	�^B	�B	��B	��B	�B	r�B	hsB	ZB	P�B	I�B	E�B	<jB	5?B	33B	0!B	2-B	.B	(�B	%�B	�B	oB	%B	  B��B��B�B�B�B�HB��B��B��BƨB�jB�9B�B��B��B��B��B��B�oB�\B�PB�B�B|�B{�B|�Bz�Bx�By�Bu�Bv�Bt�Br�Bq�Bq�Bq�Bs�Bp�Bp�Bm�Bk�BiyBgmBe`BdZBaHB]/B[#BXBL�BJ�BF�BN�BN�BJ�BF�B?}B:^B1'B/B-B-B-B/B,B.B,B)�B(�B&�B&�B%�B$�B#�B$�B#�B"�B#�B"�B!�B!�B#�B%�B#�B&�B$�B%�B#�B#�B#�B$�B"�B$�B&�B(�B)�B)�B+B)�B)�B'�B+B'�B(�B'�B'�B(�B+B(�B(�B)�B.B0!B0!B1'B2-B49B49B5?B5?B5?B8RB9XB;dB?}B@�BE�BI�BJ�BL�BO�BP�BR�BT�BVBVBW
BW
BYB[#B[#B[#B\)B]/B_;B`BBcTBgmBl�Bm�Bm�Bn�Br�Bu�Bx�B{�B� B�B�B�B�B�%B�PB�\B�bB�hB�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�!B�?B�wBÖB��B��B�B�5B�fB�mB�sB�B��B��B��B	B	JB	\B	�B	�B	�B	�B	�B	�B	 �B	#�B	"�B	 �B	 �B	$�B	(�B	)�B	-B	.B	0!B	33B	49B	6FB	9XB	:^B	=qB	=qB	?}B	A�B	C�B	H�B	G�B	F�B	J�B	L�B	M�B	N�B	R�B	VB	ZB	[#B	]/B	^5B	_;B	`BB	aHB	bNB	dZB	gmB	k�B	o�B	p�B	t�B	x�B	z�B	}�B	�B	�B	�B	�+B	�DB	�\B	�bB	�hB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�!B	�'B	�'B	�-B	�9B	�?B	�FB	�LB	�RB	�RB	�XB	�dB	�dB	�dB	�jB	�qB	�wB	�}B	��B	B	ÖB	ÖB	ĜB	ĜB	ĜB	ŢB	ƨB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�B	�B	�)B	�5B	�;B	�BB	�BB	�BB	�HB	�HB	�NB	�TB	�ZB	�`B	�fB	�mB	�sB	�yB	�yB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
%B
%B
B
%B
%B
B
B
B
B
B
%B
%B
%B
+B
1B
1B
1B
1B
1B
	7B
	7B
	7B

=B

=B

=B

=B

=B
DB
DB
DB
JB
PB
VB
\B
bB
bB
oB
oB
hB
oB
oB
uB
uB
uB
uB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
"�B
"�B
"�B
#�B
$�B
%�B
%�B
%�B
%�B
%�B
&�B
&�B
&�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
+B
+B
+B
+B
,B
,B
,B
,B
-B
.B
.B
.B
/B
0!B
0!B
0!B
0!B
0!B
/B
0!B
0!B
2-B
2-B
2-B
2-B
2-B
33B
33B
49B
49B
49B
49B
49B
6FB
7LB
7LB
7LB
7LB
8RB
8RB
8RB
8RB
8RB
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
9XB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
:^B
;dB
;dB
;dB
<jB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
?}B
?}B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
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
C�B
C�B
D�B
E�B
E�B
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
H�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
N�B
N�B
N�B
O�B
O�B
O�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
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
T�B
T�B
T�B
T�B
T�B
VB
W
B
W
B
W
B
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
[#B
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
]/B
\)B
]/B
]/B
]/B
]/B
]/B
]/B
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
aHB
aHB
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
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
aHB
bNB
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
cTB
cTB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
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
gmB
gmB
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
j111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B$�B%�B(�B/�B1�B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B2�B.�B*�B?HBP�BX�BX�B[�Be,Bh>BoiBr|Bt�Bt�Br|Bu�Bw�B{�B|�B{�B|�By�Bx�B{�B~�B~�B}�B~�Bz�Bu�BlWBh>BlWBr|Bu�By�B|�B}�B~�B�Bu�Bf2Be,B\�BV�BK�BB[B>(B:*B1�B-�B�BFB	B��B�]B��B�rB��B��B�FB��BjKB;B�B
�B
�gB
��B
�,B
�	B
��B
�B
y�B
r|B
oiB
lWB
jKB
f2B
]�B
S�B
JrB
EmB
?HB
1�B
+�B
'�B
%�B
#�B
�B
dB
]B
EB
:B
 �B	�[B	�*B	��B	�zB	�4B	�B	��B	��B	�KB	��B	raB	h>B	Y�B	P�B	IlB	ESB	<B	5B	2�B	/�B	1�B	-�B	(�B	%�B	KB	 B	�B��B��B��B�hB�B�UB��B��BϫB�rB�YB�B�B��B��B��B�kB�eB�YB� B�B�B��B��B|�B{�B|�Bz�Bx�By�ButBvzBtnBraBqvBq[BqvBs�BpoBpUBmCBkQBi*BgBeBdB`�B\�BZ�BW�BL~BJrBFYBN�BN�BJrBFYB?.B:B0�B.�B,�B,�B,�B.�B+�B-�B+�B)�B(�B&�B&�B%�B$�B#�B$�B#�B"�B#�B"�B!�B!|B#�B%�B#�B&�B$�B%�B#�B#�B#�B$�B"�B$�B&�B(�B)�B)�B*�B)�B)�B'�B*�B'�B(�B'�B'�B(�B*�B(�B(�B)�B-�B/�B/�B0�B1�B3�B3�B4�B4�B4�B8B9	B;B?.B@4BESBIlBJrBL~BO�BP�BR�BT�BU�BU�BV�BV�BX�BZ�BZ�BZ�B[�B\�B^�B_�Bb�BgBl=BmCBmCBnIBraButBx�B{�B�B��B��B��B��B��B�B�B�B�B� B�B�9B�EB�QB�QB�WB�WB�CB�=B�dB��B�tB��B��B��B��B��B��B��B�B�GB̈́BөB��B��B�B�B�$B�MB�zB��B��B	�B	�B	B	2B	2B	B	?B	EB	QB	 vB	#�B	"�B	 vB	 vB	$tB	(�B	)�B	,�B	-�B	/�B	2�B	3�B	5�B	9	B	:B	="B	="B	?.B	A;B	C-B	HKB	GEB	FYB	JrB	LdB	M�B	N�B	R�B	U�B	Y�B	Z�B	\�B	]�B	^�B	_�B	`�B	a�B	dB	gB	k6B	o5B	pUB	tnB	x�B	z�B	}�B	��B	��B	��B	��B	��B	�B	�B	� B	� B	�,B	�2B	�?B	�EB	�QB	�dB	�pB	�\B	�|B	�nB	��B	�tB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�	B	�B	�B	�B	�B	�B	�(B	�.B	�;B	�AB	�GB	�GB	�MB	�MB	�MB	�SB	�YB	�YB	�_B	�lB	�rB	�rB	�rB	�xB	�jB	�jB	̈́B	ΊB	ϑB	уB	өB	յB	ּB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�B	�B	�$B	�*B	�*B	�$B	�$B	�0B	�=B	�=B	�=B	�=B	�IB	�IB	�OB	�OB	�OB	�UB	�UB	�UB	�aB	�aB	�aB	�hB	�MB	�hB	�hB	�hB	�hB	�nB	�nB	�TB	�TB	�zB	��B	��B	��B	��B	��B	�xB	��B	��B	��B	��B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	�B
	�B
	�B
	�B
	�B

�B

�B

�B
�B
B
B
B
B
�B
 B
 B
B
 B
 B
B
&B
&B
&B
B
,B
,B
2B
2B
9B
9B
9B
?B
EB
EB
EB
KB
7B
QB
WB
WB
CB
dB
dB
dB
dB
dB
dB
jB
OB
jB
jB
pB
pB
pB
pB
 \B
 vB
!bB
"�B
"�B
"�B
#�B
$�B
%�B
%�B
%�B
%�B
%�B
&�B
&�B
&�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
*�B
*�B
*�B
*�B
+�B
+�B
+�B
+�B
,�B
-�B
-�B
-�B
.�B
/�B
/�B
/�B
/�B
/�B
.�B
/�B
/�B
1�B
1�B
1�B
1�B
1�B
2�B
2�B
3�B
3�B
3�B
3�B
3�B
5�B
6�B
6�B
6�B
6�B
8B
8B
8B
8B
8B
7�B
8B
8B
9	B
8�B
9	B
9	B
9	B
9	B
9	B
8�B
9	B
9	B
8�B
8�B
8�B
8�B
9	B
8�B
:B
:B
:B
:B
;B
;B
;B
<B
="B
="B
="B
="B
>(B
>(B
>(B
?.B
?.B
@4B
@4B
@4B
@4B
@4B
A;B
A;B
A B
A;B
A;B
A;B
BAB
BAB
BAB
B'B
CGB
C-B
CGB
CGB
CGB
DMB
ESB
ESB
E9B
ESB
ESB
ESB
ESB
ESB
FYB
FYB
G_B
G_B
G_B
GEB
G_B
HKB
IlB
IlB
IlB
IlB
IlB
IlB
IRB
IlB
IlB
IlB
JrB
JrB
JrB
JrB
JrB
JrB
JrB
KxB
KxB
KxB
L~B
L~B
LdB
L~B
L~B
L~B
L~B
L~B
M�B
MjB
M�B
N�B
N�B
N�B
OvB
O�B
O�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
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
T�B
T�B
T�B
T�B
T�B
U�B
V�B
V�B
V�B
W�B
W�B
X�B
X�B
X�B
X�B
X�B
X�B
Y�B
Y�B
Y�B
Z�B
Z�B
Z�B
Z�B
Z�B
[�B
[�B
[�B
[�B
[�B
[�B
[�B
[�B
\�B
[�B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
]�B
]�B
]�B
]�B
]�B
]�B
]�B
]�B
]�B
^B
]�B
]�B
_B
^�B
^�B
^�B
^�B
_�B
_�B
_�B
_�B
_�B
aB
`�B
`�B
`�B
`�B
`�B
`�B
a�B
a�B
a�B
a�B
a�B
a�B
a�B
a�B
a�B
a�B
a�B
a�B
bB
a�B
a�B
a�B
a�B
a�B
`�B
bB
a�B
a�B
bB
a�B
a�B
c B
cB
cB
cB
cB
cB
cB
cB
cB
cB
cB
cB
dB
c�B
dB
dB
dB
dB
dB
dB
e,B
eB
e,B
fB
fB
fB
fB
fB
gB
gB
gB
gB
h$B
h$B
h$B
h$B
h$B
h$B
iDB
i*B
i*B
i*B
i*B
i*B
j0B
j0111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    SP=-0.62(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201810030048122018100300481220181003004812201810040041062018100400410620181004004106JA  ARFMdecpA19c                                                                20180928033513  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180927183522  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180927183523  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180927183523  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180927183524  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180927183524  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180927183524  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180927183524  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180927183524  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180927183524                      G�O�G�O�G�O�                JA  ARUP                                                                        20180927185637                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180928153816  CV  JULD            G�O�G�O�F�%)                JM  ARCAJMQC2.0                                                                 20181002154812  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20181002154812  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20181003154106  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200120021520                      G�O�G�O�G�O�                