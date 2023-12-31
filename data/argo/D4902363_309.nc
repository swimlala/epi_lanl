CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-12-10T00:37:34Z creation;2018-12-10T00:37:40Z conversion to V3.1;2019-12-19T07:26:05Z update;     
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
_FillValue                 �  I8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M    PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p@   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t(   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �H   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �0   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �P   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ݈   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181210003734  20200115131517  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL              5A   JA  I2_0576_309                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @ؖ�xj1�1   @ؖ�l��@91&�x���d2��S&1   GPS     A   B   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @���@�  A��A   A@  A^ffA~ffA�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ�fD[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Da��Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�3D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ DԼ�D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D��3D�3D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�C3D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�{@�z�@��A=qA>=qA\��A|��A��A��A��A��A��A��A��A��B�\B�\B�\B�\B'�\B/�\B7�\B?�\BG�\BO�\BW�\B_�\Bg�\Bo�\Bw�\B�\B�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮC��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D x�D ��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dr�D��D	x�D	��D
x�D
��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��D x�D ��D!x�D!��D"x�D"��D#x�D#��D$x�D$��D%x�D%��D&x�D&��D'x�D'��D(x�D(��D)x�D)��D*x�D*��D+x�D+��D,x�D,��D-x�D-��D.x�D.��D/x�D/��D0x�D0��D1x�D1��D2x�D2��D3x�D3��D4x�D4��D5x�D5��D6x�D6��D7x�D7��D8x�D8��D9x�D9��D:x�D:��D;x�D;��D<x�D<��D=x�D=��D>x�D>��D?x�D?��D@x�D@��DAx�DA��DBx�DB��DCx�DC��DDx�DD��DEx�DE��DFx�DF��DGx�DG��DHx�DH��DIx�DI��DJx�DJ��DKx�DK��DLx�DL��DMx�DM��DNx�DN��DOx�DO��DPx�DP��DQx�DQ��DRx�DR��DSx�DS��DTx�DT��DUx�DU��DVx�DV��DWx�DW��DXx�DX��DYx�DY��DZ\DZ��D[x�D[��D\x�D\��D]x�D]��D^x�D^��D_x�D_��D`x�D`��Dax�Da�Dbx�Db��Dcx�Dc��Ddx�Dd��Dex�De��Dfx�Df��Dgx�Dg��Dhx�Dh��Dix�Di��Djx�Dj��Dkx�Dk��Dlx�Dl��Dmx�Dm��Dnx�Dn��Dox�Do��Dpx�Dp��Dqx�Dq��Drx�Dr��Dsx�Ds��Dtx�Dt��Dux�Du��Dvx�Dv��Dwx�Dw��Dxx�Dx��Dyx�Dy��Dzx�Dz��D{x�D{��D|x�D|��D}x�D}��D~x�D~��Dx�D��D�<{D�|{D��{D��{D�<{D�|{D��{D���D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�9HD�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D���D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D¼{D��{D�<{D�|{Dü{D��{D�<{D�|{Dļ{D��{D�<{D�|{Dż{D��{D�<{D�|{DƼ{D��{D�<{D�|{DǼ{D��{D�<{D�|{Dȼ{D��{D�<{D�|{Dɼ{D��{D�<{D�|{Dʼ{D��{D�<{D�|{D˼{D��{D�<{D�|{D̼{D��{D�<{D�|{Dͼ{D��{D�<{D�|{Dμ{D��{D�<{D�|{Dϼ{D��{D�<{D�|{Dм{D��{D�<{D�|{DѼ{D���D�<{D�|{DҼ{D��{D�<{D�|{DӼ{D��{D�<{D�|{DԹHD��{D�<{D�|{Dռ{D��{D�<{D�|{Dּ{D��{D�<{D�|{D׼{D��{D�<{D�|{Dؼ{D��{D�<{D�|{Dټ{D��{D�<{D�|{Dڼ{D��{D�<{D�|{Dۼ{D��{D�<{D�|{Dܼ{D��{D�<{D�|{Dݿ�D���D�<{D�|{D޼{D��{D�<{D�|{D߼{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�HD��HD�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D��{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�?�D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D���D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��PA��DA��+A��hA���A��uA��hA��DA��+A��+A�~�A�z�A�~�A��A��+A��7A��+A�z�A�x�A�r�A�E�A��A��A�+A��A��HA��#A���A�A��wA��jA��RA��9A��A���A���A��PA�VA��jA�7LA��A�A��A�7LA���A��A���A��A�ȴA�\)A���A��mA���A��;A�S�A��!A��A��A��A�z�A�VA���A�E�A�"�A���A��mA��/A��A�t�A�9XA��A�jA��HA�9XA��
A��RA�oA�dZA�$�A��HA���A��!A�G�A�E�A�1A��A�O�A��^A���A��A��A��-A�p�A�VA���A�  A���A��uA��A���A�ZA�M�A�ƨA��A���A�A}7LA{C�AyXAxM�Av�AuoAt^5AsVAr�Aq�hAp��Ao�An��Am�TAm%Ak�^Aj5?AhbAf�Ae�#Ae"�Ac�7Ab��AbM�Aa��A_��A]33AZ=qAX�\AXAW�AU�TAU%AS�TAQ�mAP�RAOC�AN=qAM�AL1'AJM�AHM�AGp�AF^5AEp�AD��ADZAB��A@��A@A�A?�A?7LA>~�A<�HA<5?A:�!A9�wA8�/A8n�A8bA7ƨA6�`A6�DA6=qA5p�A3�A2~�A1�^A1;dA0��A/��A/XA.�+A-��A-K�A+�A*�A)"�A(I�A'��A&M�A%p�A$��A$n�A$ZA#��A#�A"�A"jA" �A!�A!�;A!�TA!��A!/AC�AE�AAx�A7LA�yA-A�PA+A��A��A �A��A��AK�A  A��A��A�RA\)A^5A?}A�RA$�A��AG�A�/A&�A��A��A/A
�!A
-A	hsA�yA �A�A�^Al�A/A�AA�AVA�A��A��A`BA"�A ��A �RA �\A ~�A ZA �@���@��@��@���@�?}@�r�@��@���@�"�@��^@�Ĝ@�|�@陚@�9X@���@�?}@��`@�u@�w@�~�@�9@�"�@�J@��m@�@�(�@ְ!@ա�@��/@�b@�K�@Ұ!@��T@�G�@���@�Ĝ@�9X@Ο�@�?}@�1'@˝�@�33@��y@ʸR@ʗ�@�$�@�@��@�@ũ�@�&�@�bN@°!@��7@�`B@�r�@�+@���@�ff@��^@��@�p�@���@�ff@��@�I�@��;@�ȴ@��h@��@��9@�1'@� �@��@��F@�|�@�;d@�@��+@��@�O�@��`@��D@�b@�l�@��y@���@�X@��@��`@�Ĝ@�A�@�33@�E�@�x�@�Ĝ@��@��T@��@��@���@�bN@��@���@���@�K�@���@�=q@�@��@���@�I�@��m@���@�@�?}@�Ĝ@�b@�S�@�
=@��R@��@�/@���@��u@��D@�r�@��@�~�@�hs@��@� �@��@���@���@��/@�Q�@� �@���@�\)@��R@�V@�=q@�=q@�5?@�E�@�=q@�5?@�{@��h@�X@��`@�j@�ƨ@�;d@��@��!@���@�ff@���@���@�J@�J@��@�@�X@�?}@���@���@�bN@�(�@��m@��F@���@��@�t�@�S�@�
=@��@��!@��\@�ff@�$�@�$�@��@��@�J@��#@���@�hs@�%@�Ĝ@�j@�1'@���@���@��y@��R@��!@���@��+@�~�@�V@��@�{@��@��^@�x�@���@�j@� �@�;@�w@��@�@l�@K�@~�y@~�+@~ff@~5?@}�@}�T@}?}@|Z@{dZ@z�\@zJ@y7L@w��@v��@v5?@u�@tI�@s��@t��@tZ@r��@q��@qx�@p��@p1'@pb@o�;@o�w@o�@nv�@nE�@nE�@nE�@n5?@m�@m@m��@m�-@m��@m��@m��@m�@m�@m�@n@nE�@n��@o�@nȴ@nȴ@nV@m�-@m�@m?}@l��@k�
@kS�@j�@j~�@j^5@i�7@h �@g\)@g
=@f�R@f�R@f��@f��@fE�@d(�@b=q@a��@a�^@a��@ax�@aX@a&�@`��@`��@`�u@`r�@`A�@`b@`  @_�@_�@_�;@_�@_l�@_�@_
=@_
=@^�R@^��@^v�@^�+@^��@^ff@]��@]?}@\�@\z�@\1@[��@[�F@[��@[t�@[S�@[33@[o@Z��@Z�\@Z-@Y�#@Y�^@Y�7@Y7L@X��@X�@XbN@W�@V�+@VE�@V$�@U�T@U�h@UV@T�@Tz�@T(�@S��@SS�@S33@R��@Q�^@Qhs@P1'@O|�@O\)@O;d@O�@Nȴ@Nff@N$�@N@M@M�@M�@M�@M`B@L�D@L(�@K��@K�@J��@I�#@Ix�@I&�@H��@Hr�@H  @G�w@G|�@G
=@E�-@D1@C@B��@B�\@BM�@B-@A�@A7L@@�@@  @>�R@>V@>@=�-@=/@<�j@;�
@;C�@;33@;"�@;"�@;"�@;"�@;o@;o@:��@:~�@:n�@:^5@:M�@9��@9�@9��@9hs@9&�@9&�@9&�@97L@97L@8��@8�u@8Ĝ@8�9@8�u@8�u@8�9@8r�@8 �@7�@7�@7�;@7��@7�P@7\)@7;d@6�y@6�y@6ȴ@6�R@6��@6�@6v�@6{@5�T@5�@5�T@5��@5��@5@5�-@5�-@5��@5�-@5��@5��@5��@5p�@5O�@5O�@5V@4�@4�j@41@3��@3S�@3o@2��@1�7@0Q�@0A�@0A�@01'@01'@0  @/��@/�@/��@/|�@/l�@/K�@.�y@.V@-�-@-V@,z�@,(�@,1@+�m@+�F@+�@+o@*�H@*��@*-@)�#@)��@)x�@)X@)&�@)�@)%@(�`@(��@(bN@(1'@'�@'�w@'|�@';d@'
=@&�y@&�R@&ff@%�-@%V@$z�@$I�@#�
@#S�@#o@"�@"M�@"J@"J@!��@!�@!��@!X@ �`@ �@ bN@ A�@ 1'@�;@��@K�@�@�@��@v�@v�@V@�T@��@�m@�F@��@�@dZ@dZ@t�@dZ@C�@33@33@@�@�H@�!@J@X@7L@�@��@Ĝ@�9@�9@��@�u@bN@A�@A�@ �@  @��@l�@K�@K�@+@��@�R@v�@5?@{@@�T@�T@@p�@/@�@�@��@C�@@@@@@@@�\@��@��@�@��@�7@X@�@�`@��@��@��@Ĝ@Ĝ@Ĝ@�9@��@�u@�@bN@r�@r�@�@�@r�@bN@bN@bN@r�@r�@bN@A�@1'@ �@ �@b@b@  @  @�@��@��@ff@$�@@�T@��@@�-@��@��@�h@�h@�@�@�@�@�@�@�@�@�@�@p�@`B@`B@O�@/@V@��@��@��@Z@��@��@33@@@@
�@
�@
�@
�@
�@
�H@
��@
��@
��@
�H@
�H@
��@
��@
��@
��@
��@
�!@
�!@
�\@
n�@
M�@
=q@
-@
-@
=q@
=q@
=q@
=q@
-@
�@
J@	��@	�@	��@	�^@	G�@	�@��@��@�w@�P@+@�y@��@�+@�+@v�@V@V@5?@@@�h@�@�@p�@�@�@��@��@z�@Z111111111111111111111111111111111111144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��PA��DA��+A��hA���A��uA��hA��DA��+A��+A�~�A�z�A�~�A��A��+A��7A��+A�z�A�x�A�r�A�E�A��A��A�+A��A��HA��#A���A�A��wA��jA��RA��9A��A���A���A��PG�O�G�O�A�7LA��A�A��A�7LA���A��A���A��A�ȴA�\)A���A��mA���A��;A�S�A��!A��A��A��A�z�A�VA���A�E�A�"�A���A��mA��/A��A�t�A�9XA��A�jA��HA�9XA��
A��RA�oA�dZA�$�A��HA���A��!A�G�A�E�A�1A��A�O�A��^A���A��A��A��-A�p�A�VA���A�  A���A��uA��A���A�ZA�M�A�ƨA��A���A�A}7LA{C�AyXAxM�Av�AuoAt^5AsVAr�Aq�hAp��Ao�An��Am�TAm%Ak�^Aj5?AhbAf�Ae�#Ae"�Ac�7Ab��AbM�Aa��A_��A]33AZ=qAX�\AXAW�AU�TAU%AS�TAQ�mAP�RAOC�AN=qAM�AL1'AJM�AHM�AGp�AF^5AEp�AD��ADZAB��A@��A@A�A?�A?7LA>~�A<�HA<5?A:�!A9�wA8�/A8n�A8bA7ƨA6�`A6�DA6=qA5p�A3�A2~�A1�^A1;dA0��A/��A/XA.�+A-��A-K�A+�A*�A)"�A(I�A'��A&M�A%p�A$��A$n�A$ZA#��A#�A"�A"jA" �A!�A!�;A!�TA!��A!/AC�AE�AAx�A7LA�yA-A�PA+A��A��A �A��A��AK�A  A��A��A�RA\)A^5A?}A�RA$�A��AG�A�/A&�A��A��A/A
�!A
-A	hsA�yA �A�A�^Al�A/A�AA�AVA�A��A��A`BA"�A ��A �RA �\A ~�A ZA �@���@��@��@���@�?}@�r�@��@���@�"�@��^@�Ĝ@�|�@陚@�9X@���@�?}@��`@�u@�w@�~�@�9@�"�@�J@��m@�@�(�@ְ!@ա�@��/@�b@�K�@Ұ!@��T@�G�@���@�Ĝ@�9X@Ο�@�?}@�1'@˝�@�33@��y@ʸR@ʗ�@�$�@�@��@�@ũ�@�&�@�bN@°!@��7@�`B@�r�@�+@���@�ff@��^@��@�p�@���@�ff@��@�I�@��;@�ȴ@��h@��@��9@�1'@� �@��@��F@�|�@�;d@�@��+@��@�O�@��`@��D@�b@�l�@��y@���@�X@��@��`@�Ĝ@�A�@�33@�E�@�x�@�Ĝ@��@��T@��@��@���@�bN@��@���@���@�K�@���@�=q@�@��@���@�I�@��m@���@�@�?}@�Ĝ@�b@�S�@�
=@��R@��@�/@���@��u@��D@�r�@��@�~�@�hs@��@� �@��@���@���@��/@�Q�@� �@���@�\)@��R@�V@�=q@�=q@�5?@�E�@�=q@�5?@�{@��h@�X@��`@�j@�ƨ@�;d@��@��!@���@�ff@���@���@�J@�J@��@�@�X@�?}@���@���@�bN@�(�@��m@��F@���@��@�t�@�S�@�
=@��@��!@��\@�ff@�$�@�$�@��@��@�J@��#@���@�hs@�%@�Ĝ@�j@�1'@���@���@��y@��R@��!@���@��+@�~�@�V@��@�{@��@��^@�x�@���@�j@� �@�;@�w@��@�@l�@K�@~�y@~�+@~ff@~5?@}�@}�T@}?}@|Z@{dZ@z�\@zJ@y7L@w��@v��@v5?@u�@tI�@s��@t��@tZ@r��@q��@qx�@p��@p1'@pb@o�;@o�w@o�@nv�@nE�@nE�@nE�@n5?@m�@m@m��@m�-@m��@m��@m��@m�@m�@m�@n@nE�@n��@o�@nȴ@nȴ@nV@m�-@m�@m?}@l��@k�
@kS�@j�@j~�@j^5@i�7@h �@g\)@g
=@f�R@f�R@f��@f��@fE�@d(�@b=q@a��@a�^@a��@ax�@aX@a&�@`��@`��@`�u@`r�@`A�@`b@`  @_�@_�@_�;@_�@_l�@_�@_
=@_
=@^�R@^��@^v�@^�+@^��@^ff@]��@]?}@\�@\z�@\1@[��@[�F@[��@[t�@[S�@[33@[o@Z��@Z�\@Z-@Y�#@Y�^@Y�7@Y7L@X��@X�@XbN@W�@V�+@VE�@V$�@U�T@U�h@UV@T�@Tz�@T(�@S��@SS�@S33@R��@Q�^@Qhs@P1'@O|�@O\)@O;d@O�@Nȴ@Nff@N$�@N@M@M�@M�@M�@M`B@L�D@L(�@K��@K�@J��@I�#@Ix�@I&�@H��@Hr�@H  @G�w@G|�@G
=@E�-@D1@C@B��@B�\@BM�@B-@A�@A7L@@�@@  @>�R@>V@>@=�-@=/@<�j@;�
@;C�@;33@;"�@;"�@;"�@;"�@;o@;o@:��@:~�@:n�@:^5@:M�@9��@9�@9��@9hs@9&�@9&�@9&�@97L@97L@8��@8�u@8Ĝ@8�9@8�u@8�u@8�9@8r�@8 �@7�@7�@7�;@7��@7�P@7\)@7;d@6�y@6�y@6ȴ@6�R@6��@6�@6v�@6{@5�T@5�@5�T@5��@5��@5@5�-@5�-@5��@5�-@5��@5��@5��@5p�@5O�@5O�@5V@4�@4�j@41@3��@3S�@3o@2��@1�7@0Q�@0A�@0A�@01'@01'@0  @/��@/�@/��@/|�@/l�@/K�@.�y@.V@-�-@-V@,z�@,(�@,1@+�m@+�F@+�@+o@*�H@*��@*-@)�#@)��@)x�@)X@)&�@)�@)%@(�`@(��@(bN@(1'@'�@'�w@'|�@';d@'
=@&�y@&�R@&ff@%�-@%V@$z�@$I�@#�
@#S�@#o@"�@"M�@"J@"J@!��@!�@!��@!X@ �`@ �@ bN@ A�@ 1'@�;@��@K�@�@�@��@v�@v�@V@�T@��@�m@�F@��@�@dZ@dZ@t�@dZ@C�@33@33@@�@�H@�!@J@X@7L@�@��@Ĝ@�9@�9@��@�u@bN@A�@A�@ �@  @��@l�@K�@K�@+@��@�R@v�@5?@{@@�T@�T@@p�@/@�@�@��@C�@@@@@@@@�\@��@��@�@��@�7@X@�@�`@��@��@��@Ĝ@Ĝ@Ĝ@�9@��@�u@�@bN@r�@r�@�@�@r�@bN@bN@bN@r�@r�@bN@A�@1'@ �@ �@b@b@  @  @�@��@��@ff@$�@@�T@��@@�-@��@��@�h@�h@�@�@�@�@�@�@�@�@�@�@p�@`B@`B@O�@/@V@��@��@��@Z@��@��@33@@@@
�@
�@
�@
�@
�@
�H@
��@
��@
��@
�H@
�H@
��@
��@
��@
��@
��@
�!@
�!@
�\@
n�@
M�@
=q@
-@
-@
=q@
=q@
=q@
=q@
-@
�@
J@	��@	�@	��@	�^@	G�@	�@��@��@�w@�P@+@�y@��@�+@�+@v�@V@V@5?@@@�h@�@�@p�@�@�@��@��@z�@Z111111111111111111111111111111111111144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BH�BH�BI�BI�BH�BH�BH�BH�BI�BI�BK�BK�BK�BJ�BJ�BI�BH�BK�BJ�BI�BR�B^5B^5BXB[#Bl�Bm�Bq�Bt�Bu�Bv�Bx�Bz�B|�B|�By�BiyBE�B��BuBcTB9XB5?B9XB[#B8RBDB�BH�BB�B/B�B.B0!B(�B�BuB�B�B�B{B
=B�sB�BB�B�5B�B�yB�yB�5B��BƨB�XB��B��B�-B��B�-B�B��B�hB�7B��B�+BcTBO�B�BoB�B%�BDB�B�B
��B
�TB
�B
�3B
�'B
��B
�%B
�1B
�B
x�B
`BB
O�B
F�B
:^B
33B
2-B
(�B
�B
 �B
�B
DB
�B
	7B	��B	��B	�B	�ZB	��B	B	�RB	�dB	��B	�^B	��B	��B	��B	�oB	y�B	gmB	^5B	jB	y�B	p�B	gmB	cTB	\)B	H�B	K�B	C�B	8RB	49B	,B	�B		7B	�B	VB	oB	\B	B�B�HB�B�B�B�NBɺB�BŢB��B��B��B��B��BƨBɺBǮB�XB��B��B�B�3B�B��B��B��B��B��B�%B~�Bt�B�B�Bw�B}�B�B�DB�=B�Bv�B�B� B�B�B�B~�Bu�BcTBI�BYBjBm�BjBffB]/B_;BaHBcTB[#BS�BN�BB�B8RB7LBQ�BI�B8RB+B2-B7LB@�B?}B>wB>wB5?B�B+BA�B8RB5?B,B&�B1B2-B?}BA�B=qB<jB8RB-B#�B&�B=qB<jB;dB9XB;dB9XB:^B9XB49B-B�BuBbB�yB�B+B.B/B'�B�B!�B�B�B�B�B-B0!B-B$�B�B�B�B�BbB�B�B#�B(�B.B.B/B2-B1'B49B7LB5?B/B%�B'�B2-B6FB:^B;dB<jB;dB7LB49B1'B&�B/B8RB49B,B33B=qB7LB49B?}BA�B>wBC�B@�B7LB)�B7LBH�BK�BI�BK�BW
BZBYB^5B]/B]/B]/B\)B\)B[#BZB[#B^5B`BB_;B_;BaHBbNBffBl�Bm�Bl�BgmBbNBgmBjBl�Be`Bs�B� B�B�B�B�B�7B�1B�+B�+B�1B�PB�DB�=B�VB�bB�=B�uB��B��B��B��B��B��B��B��B�B�!B�-B�B��B��B�B�?B�9B�XB�jB�dBÖB��B�B�/B�/B�;B�mB�B�B�B�B��B��B��B��B	B	  B	B	B	1B	bB	{B	�B	�B	�B	!�B	%�B	%�B	%�B	&�B	%�B	)�B	(�B	-B	/B	1'B	33B	5?B	8RB	9XB	9XB	8RB	8RB	;dB	<jB	=qB	>wB	?}B	E�B	G�B	L�B	N�B	N�B	N�B	Q�B	Q�B	VB	W
B	YB	[#B	\)B	]/B	iyB	m�B	m�B	m�B	n�B	m�B	o�B	t�B	r�B	s�B	t�B	r�B	w�B	z�B	|�B	� B	�B	�B	� B	�B	�B	�B	�+B	�1B	�7B	�=B	�1B	�JB	�hB	�{B	��B	��B	�{B	��B	��B	��B	��B	��B	�B	�B	��B	�B	�B	�B	�!B	�'B	�!B	�!B	�B	�B	�'B	�9B	�?B	�?B	�FB	�LB	�XB	�^B	�dB	�dB	�dB	�jB	�jB	�jB	�qB	�}B	�}B	ÖB	ÖB	ĜB	ÖB	ÖB	ƨB	ƨB	ŢB	ĜB	ȴB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�
B	��B	��B	��B	�;B	�NB	�TB	�NB	�TB	�TB	�TB	�ZB	�`B	�`B	�`B	�fB	�mB	�mB	�mB	�mB	�fB	�fB	�mB	�yB	�sB	�mB	�B	�B	�B	�B	�sB	�`B	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B	��B
B
+B
+B
+B
%B
%B
1B
1B
1B
	7B
DB
DB
	7B
+B
	7B
DB

=B
1B

=B
VB
\B
\B
\B
\B
hB
bB
VB
DB
DB
oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
�B
�B
�B
�B
!�B
&�B
&�B
'�B
&�B
&�B
&�B
&�B
%�B
%�B
'�B
'�B
'�B
&�B
'�B
'�B
'�B
(�B
)�B
+B
)�B
)�B
'�B
(�B
,B
+B
)�B
+B
,B
(�B
(�B
)�B
,B
+B
+B
,B
,B
,B
,B
.B
.B
.B
/B
/B
-B
-B
/B
1'B
1'B
0!B
1'B
0!B
1'B
1'B
1'B
1'B
1'B
1'B
0!B
0!B
0!B
0!B
0!B
/B
/B
-B
/B
1'B
1'B
0!B
-B
1'B
:^B
;dB
;dB
;dB
:^B
:^B
;dB
;dB
;dB
;dB
:^B
9XB
8RB
8RB
:^B
;dB
>wB
?}B
?}B
?}B
>wB
=qB
>wB
>wB
>wB
?}B
@�B
@�B
A�B
A�B
B�B
B�B
A�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
C�B
B�B
@�B
?}B
A�B
B�B
E�B
E�B
E�B
G�B
H�B
G�B
I�B
K�B
J�B
J�B
I�B
H�B
H�B
J�B
L�B
L�B
L�B
K�B
L�B
L�B
M�B
M�B
N�B
O�B
O�B
N�B
L�B
J�B
L�B
S�B
T�B
VB
VB
VB
VB
VB
VB
VB
VB
T�B
VB
T�B
S�B
R�B
R�B
W
B
XB
XB
YB
ZB
ZB
ZB
ZB
ZB
ZB
[#B
ZB
ZB
ZB
ZB
[#B
\)B
[#B
[#B
ZB
[#B
[#B
\)B
]/B
]/B
]/B
\)B
[#B
[#B
]/B
\)B
ZB
S�B
^5B
aHB
aHB
aHB
aHB
`BB
_;B
]/B
^5B
bNB
bNB
aHB
bNB
bNB
aHB
cTB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
e`B
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
dZB
dZB
e`B
e`B
e`B
dZB
dZB
cTB
aHB
`BB
cTB
dZB
e`B
ffB
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
hsB
hsB
iyB
iyB
iyB
hsB
hsB
hsB
hsB
hsB
hsB
gmB
gmB
gmB
ffB
ffB
ffB
ffB
ffB
hsB
jB
k�B
k�B
k�B
k�B
k�B
l�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
k�B
k�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
k�B
jB
k�B
k�B
jB
hsB
m�B
m�B
n�B
n�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
o�B
p�B
q�B
q�B
q�B
p�B
q�B
r�B
r�B
s�B
s�B
s�111111111111111111111111111111111111144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BH�BH�BI�BI�BH�BH�BH�BH�BI�BI�BK�BK�BK�BJ�BJ�BI�BH�BK�BJ�BJ#BS@B^B^5BXyB[�Bl�Bm�Bq�Bt�Bu�Bv�Bx�Bz�B}"B}<BzDBj�G�O�G�O�B�Be�B>(B9>B<�B]B<B�B vBI�BDB1[B!�B/�B1�B*�B�B�BIB�B�B�B^B�6B�BۦB�vB�=B�B�0B�;B�@B�KB�JB��B�IB��B�QB��B��B��B�@B�=B��B�Be�BRoB"�BmB)B'BjBB$B
��B
�2B
�B
��B
�hB
��B
�7B
�rB
��B
z�B
c B
R�B
I�B
<�B
5ZB
3�B
*�B
�B
!�B
=B
�B
B

XB	��B	��B	��B	�B	ּB	ĶB	��B	�"B	��B	�B	��B	��B	��B	��B	|�B	j�B	a�B	lWB	z�B	q�B	h�B	d�B	]�B	KB	MPB	ESB	9�B	5�B	-�B	B	�B	�B	�B	�B	.B	?B�B�B�B�B�UB�B��B��B��B�B��BԯBՁB҉B��B�XB�KB��B��B��B�!B�B�B�B��B��B��B�qB�1B��Bv�B�9B�gBy�BB�B��B��B��Bx8B��B��B��B�{B�aB.Bv`Bd�BL~BZQBkQBm�BkBgB^OB`Ba�Bc�B[�BT�BO�BC�B:^B8�BR:BJXB9�B,�B3�B8�BA;B@iB?cB?HB6FB!B,qBBB9rB6B-CB(XBDB2�B?�BA�B>B=B9	B.cB%`B(XB=�B<�B;�B9�B;�B9�B:�B9�B4�B-�B �B�B B�B��B+6B.}B/�B(�B�B"�B�B�B�BB-wB0�B-�B%�B�B�B�B�B B
B�B$�B)�B.�B.�B/�B2�B1�B4�B7�B5�B/�B'8B(�B2�B6�B:�B;�B<�B;�B7�B4�B2B(XB0B8�B5B-]B4B=�B88B5%B?�BA�B?BC�B@�B8B+�B8lBIRBLJBJ�BL�BW�BZ�BYB^jB]~B]dB]dB\�B\xB[�BZ�B[�B^�B`�B_�B_�Ba�Bc BgBl�Bm�Bl�Bh
Bc Bh>Bk6Bm]Bf�BtnB�OB�[B�aB��B��B�RB��B��B��B��B��B��B��B��B��B�^B�FB�#B�B�IB�;B�2B�LB��B��B�cB�oB�GB�cB��B��B��B��B��B��B�B�jB�MB�HB�kB�~BݘB��B�B��B��B��B��B��B�B�B�rB	UB	 �B	oB	�B	�B	�B	�B	�B	�B	�B	!�B	%�B	%�B	&2B	'8B	&2B	*B	)yB	-CB	/iB	1vB	3�B	5�B	8lB	9�B	9�B	8�B	8�B	;�B	<�B	=�B	>�B	?�B	E�B	G�B	L�B	N�B	OB	OBB	R B	RTB	VSB	WYB	YeB	[qB	\�B	]�B	i�B	m�B	m�B	m�B	n�B	m�B	o�B	t�B	r�B	s�B	uB	s3B	xB	{B	}<B	�4B	�B	� B	�4B	�;B	�AB	�gB	�_B	�KB	�lB	�rB	��B	��B	��B	��B	��B	�
B	�B	��B	��B	�;B	� B	�B	�B	�=B	��B	�qB	�iB	�]B	�UB	�[B	�;B	�;B	�IB	�OB	�[B	�TB	�ZB	�tB	�zB	�fB	�XB	�xB	�B	�B	�dB	�jB	�jB	��B	��B	�cB	�}B	ÖB	ðB	ĶB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�DB	�^B	�4B	�B	�B	�+B	�+B	�?B	�aB	͟B	ңB	�;B	�NB	�nB	�B	�nB	�nB	�nB	�tB	�zB	�zB	�zB	�B	�mB	�B	�B	�B	�B	�B	�B	�yB	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	��B	��B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�	B	�B	��B	�B	�$B	�B	�B	�B	�0B	�"B	�(B	�6B	�DB
 OB	�qB
[B
+B
EB
EB
YB
YB
fB
fB
fB
	RB
DB
^B
	lB
�B
	�B
^B

�B
�B

�B
�B
�B
vB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
1B
�B
 �B
�B
�B
�B
 B
"B
'B
&�B
'�B
&�B
'B
&�B
'B
&B
%�B
'�B
($B
($B
'B
($B
(
B
(
B
)B
)�B
+B
*B
*B
($B
)*B
+�B
+B
*0B
+B
,"B
)*B
)*B
*0B
,B
+6B
+6B
,=B
,"B
,"B
,"B
.B
.IB
.IB
/5B
/B
-CB
-CB
/5B
1AB
1'B
0UB
1'B
0;B
1'B
1AB
1'B
1AB
1AB
1'B
0!B
0UB
0UB
0;B
0;B
/5B
/OB
-]B
/iB
1[B
1[B
0oB
-�B
1�B
:^B
;dB
;dB
;B
:�B
:xB
;B
;B
;�B
;�B
:�B
9�B
8�B
8�B
:�B
;�B
>�B
?�B
?�B
?�B
>�B
=�B
>�B
>�B
>�B
?�B
@�B
@�B
A�B
A�B
B�B
B�B
A�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
C�B
B�B
@�B
?�B
A�B
B�B
E�B
E�B
E�B
G�B
H�B
G�B
I�B
K�B
J�B
J�B
I�B
H�B
H�B
J�B
L�B
MB
MB
K�B
MB
L�B
M�B
M�B
OB
O�B
O�B
OB
MB
K)B
M6B
T,B
UB
VB
VB
VB
VB
VB
VB
VB
VB
U2B
VB
UB
TB
S&B
S@B
W
B
X+B
XEB
Y1B
ZB
Z7B
Z7B
Z7B
ZQB
ZQB
[=B
ZQB
ZQB
ZQB
ZQB
[WB
\CB
[WB
[=B
Z7B
[WB
[=B
\]B
]IB
]dB
]IB
\]B
[=B
[WB
]IB
\]B
ZQB
T�B
^OB
abB
aHB
aHB
aHB
`BB
_VB
]dB
^�B
bNB
bhB
a|B
bNB
b�B
a|B
cnB
dtB
dtB
dZB
dZB
dtB
dtB
dZB
dtB
dtB
ezB
d�B
ezB
ezB
ezB
ezB
ezB
e`B
ezB
ezB
ezB
e`B
d�B
dtB
dZB
ezB
e`B
e`B
dtB
dtB
c�B
a�B
`�B
c�B
d�B
ezB
f�B
g�B
gmB
g�B
hsB
h�B
hsB
hsB
h�B
h�B
iyB
iyB
h�B
hsB
i�B
i�B
iyB
h�B
hsB
hsB
h�B
h�B
hsB
g�B
g�B
g�B
f�B
f�B
f�B
f�B
f�B
h�B
j�B
k�B
k�B
k�B
k�B
k�B
l�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
k�B
k�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
k�B
j�B
k�B
k�B
j�B
h�B
m�B
m�B
n�B
n�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
o�B
p�B
q�B
q�B
q�B
p�B
q�B
r�B
r�B
s�B
s�B
s�111111111111111111111111111111111111144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.11(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201812140036362018121400363620181214003636201812140200162018121402001620181214020016201812150022522018121500225220181215002252  JA  ARFMdecpA19c                                                                20181210093733  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20181210003734  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20181210003738  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20181210003738  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20181210003739  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20181210003739  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20181210003739  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcp2.8e                                                                20181210003739  QCF$                G�O�G�O�G�O�            4000JA  ARGQaqcp2.8e                                                                20181210003739  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20181210003739  QCF$                G�O�G�O�G�O�            4000JA  ARGQrqcpt16c                                                                20181210003739  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20181210003740                      G�O�G�O�G�O�                JA  ARUP                                                                        20181210005554                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20181210153207  CV  JULD            G�O�G�O�Fķ�                JM  ARCAJMQC2.0                                                                 20181213153636  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20181213153636  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20181213170016  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20181214152252  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115131517                      G�O�G�O�G�O�                