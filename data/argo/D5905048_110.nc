CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2017-04-19T00:35:39Z creation;2017-04-19T00:35:42Z conversion to V3.1;2019-12-19T08:09:53Z update;     
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
resolution        =���   axis      Z        p  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     p  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \X   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  `4   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �<   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     p  �d   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  �D   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ۴   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �D   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �D   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �D   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �D   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �$   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �(   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �8   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �<   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �@   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �DArgo profile    3.1 1.2 19500101000000  20170419003539  20200116211515  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               nA   JA  I2_0577_110                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @� �ĥ[�1   @� ���-�@34֡a���d�����1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_�fD`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dwy�Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D��D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�@ D� D��D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�6f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@x��@�z�@�z�A=qA>=qA^=qA~=qA��A��A��A��A��A��A��A��B�\B�\B�\B�\B'�\B/�\B7�\B?�\BG�\BO�\BW�\B_�\Bg�\Bo�\Bw�\B�\B�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮC��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D x�D ��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��D	x�D	��D
x�D
��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��D x�D ��D!x�D!��D"x�D"��D#x�D#��D$x�D$��D%x�D%��D&x�D&��D'x�D'��D(x�D(��D)x�D)��D*x�D*��D+x�D+��D,x�D,��D-x�D-��D.x�D.��D/x�D/��D0x�D0��D1x�D1��D2x�D2��D3x�D3��D4x�D4��D5x�D5��D6x�D6��D7x�D7��D8x�D8��D9x�D9��D:x�D:��D;x�D;��D<x�D<��D=x�D=��D>x�D>��D?x�D?��D@x�D@��DAx�DA��DBx�DB��DCx�DC��DDx�DD��DEx�DE��DFx�DF��DGx�DG��DHx�DH��DIx�DI��DJx�DJ��DKx�DK��DLx�DL��DMx�DM��DNx�DN��DOx�DO��DPx�DP��DQx�DQ��DRx�DR��DSx�DS��DTx�DT��DUx�DU��DVx�DV��DWx�DW��DXx�DX��DYx�DY��DZx�DZ��D[x�D[��D\x�D\��D]x�D]��D^x�D^��D_\D_��D`x�D`��Dax�Da��Dbx�Db��Dcx�Dc��Ddx�Dd��Dex�De��Dfx�Df��Dgx�Dg��Dhx�Dh��Dix�Di��Djx�Dj��Dkx�Dk��Dlx�Dl��Dmx�Dm��Dnx�Dn��Dox�Do��Dpx�Dp��Dqx�Dq��Drx�Dr��Dsx�Ds��Dtx�Dt��Dux�Du��Dvx�Dv��Dwr�Dw��Dxx�Dx��Dyx�Dy��Dzx�Dz��D{x�D{��D|x�D|��D}x�D}��D~x�D~��Dx�D�D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�9HD�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D���D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D��D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D¼{D��{D�<{D�|{Dü{D��{D�<{D�|{Dļ{D��{D�<{D�|{Dż{D��{D�<{D�|{DƼ{D��{D�<{D�|{DǼ{D��{D�<{D�|{Dȼ{D��{D�<{D�|{Dɼ{D��{D�<{D�|{Dʼ{D��{D�<{D�|{D˼{D��{D�<{D�|{D̼{D��{D�<{D�|{Dͼ{D��{D�<{D�|{Dμ{D��{D�<{D�|{Dϼ{D��{D�<{D�|{Dм{D��{D�<{D�|{DѼ{D��{D�<{D�|{DҼ{D��{D�<{D�|{DӼ{D��{D�<{D�|{DԼ{D��{D�<{D�|{Dռ{D��{D�<{D�|{Dּ{D��{D�<{D�|{D׼{D��{D�<{D�|{Dؼ{D��{D�<{D�|{Dټ{D��{D�<{D�|{Dڼ{D��{D�<{D�|{Dۼ{D��{D�<{D�|{Dܼ{D��{D�<{D�|{Dݼ{D��{D�<{D�|{D޼{D��{D�<{D�|{D߼{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��HD�<{D�|{D�HD��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D��{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D���D�2�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�5?A�$�A�+A� �A��A�bA�JA�A�%A�1A�A�  A���Aϴ9A��Ḁ�A��A���Aˏ\Aʡ�Aɝ�A�=qA�(�A���A�+AƶFAŝ�A�bNAģ�A�5?A�A�  A�|�A�-A��9A��A��+A�(�A��A�ȴA��A��7A�1'A��A���A�bNA�E�A� �A��A��!A�t�A�K�A�1A��^A�jA�G�A�/A�&�A�{A�A��A��wA�ffA�I�A�E�A��A���A�x�A���A�C�A�  A��A���A�K�A��mA�~�A�5?A��TA��9A�+A��-A��;A�ȴA�?}A���A�A�1A�bNA�=qA�-A�7LA���A�;dA�ZA�/A���A�C�A�I�A���A�E�A���A�K�A�"�A���A���A�VA���A�5?A���A�ƨA�bA��hA��7A���A�r�A�1'A��A���A�z�A��#A�=qA��A�x�A���A�jA��mA��A�ƨA�1'A��HA�wA}
=A{��AyVAx9XAw�Aw�AwAv��AqAo�wAodZAn�An1Al�Ah�HAe+Ac��Ac?}AaK�A]�A[�TA[/AZZAY;dAW�AU�FAQ?}AO�ANz�AM�;AK7LAH��AG�
AG+AF�RAE�AC�AA�
A?�mA>��A=��A<�/A;��A:z�A9K�A8��A7�7A6{A4n�A4  A1�A/�PA.ĜA.�A-�A,(�A*�\A'hsA%t�A$ZA#��A#A#XA!�A �!A�AjA%A��AjA��A��AJA�A��A�AK�A��A5?A�AAQ�A�TA;dAE�AoA
��A	l�A��AVAƨA"�A�9AƨA�`AM�A��A��A�\A~�A~�A��Ahs@�ƨ@�ff@��@���@�\)@���@���@��@���@��P@�V@��@�v�@��@�-@���@�^5@�x�@�`B@�-@�%@�D@�b@��@�~�@�hs@��@�$�@�^@�j@߶F@��m@��@ߕ�@�t�@�~�@�O�@�K�@ڇ+@�V@�
=@�=q@ա�@ա�@�p�@��@�bN@�K�@��@�X@��@���@�9X@��y@Ώ\@�hs@��/@̓u@̃@��;@˾w@��@��y@��@�7L@� �@�"�@Ƈ+@��@þw@�@+@�$�@��@��u@�Q�@��P@���@��\@�M�@��@��@���@�p�@��@���@�Z@�I�@�j@���@���@��H@��!@�=q@���@�9X@�|�@���@��!@�v�@�@�`B@���@��;@��@�dZ@�+@���@���@���@��+@�$�@���@��7@�X@��`@��u@�A�@� �@��m@���@�"�@���@�ȴ@���@�v�@��@�hs@���@���@��-@���@�z�@�9X@���@�l�@�@��@�bN@�r�@���@���@�dZ@��@�v�@�ȴ@��@���@��!@��+@�E�@���@��/@��/@���@��@��@�?}@��9@���@�bN@�9X@� �@��@�l�@�S�@�K�@�;d@�"�@�@��@���@�=q@��h@���@��u@�r�@�Z@�9X@� �@��m@�|�@�K�@���@�"�@�33@�C�@���@�J@��^@��T@��^@�x�@��u@�Q�@�I�@� �@��m@��
@��F@��w@��@�1'@�bN@�Z@�(�@��;@���@�33@���@��@���@�M�@�5?@���@���@�p�@�?}@�?}@�7L@��@�&�@��@��/@��D@�I�@� �@���@��F@�l�@���@���@�n�@�=q@��@�@��#@��^@���@�G�@�%@���@���@���@���@��@�A�@��;@�  @�ƨ@�;d@�+@�o@���@���@�ff@�=q@��@��7@�&�@��9@��@���@�K�@�o@�\)@�;d@�+@���@��@���@�=q@��#@��^@���@��^@�/@���@�Ĝ@��D@�I�@�1@��m@���@��@�l�@�+@��@�o@��@��!@��+@�V@��@���@�x�@�O�@�%@��@��@�j@�bN@�A�@�(�@�1@���@��m@�
=@��@���@�n�@��@��@���@���@���@��@��D@�1'@��@���@��F@���@�|�@�\)@��y@�V@��@��h@�hs@�/@�V@���@���@�9X@~��@~�@~�R@~ff@}�@}�@|�D@|(�@{��@{t�@z�H@zn�@zJ@yhs@xĜ@xbN@xQ�@xb@w�@v��@vE�@u�T@u�@t�@s�m@s33@r�!@r=q@q�^@q&�@p�`@p��@p1'@o��@ol�@o
=@n��@m�@m�@l��@lz�@l(�@k��@j�H@j=q@i�@i��@ihs@i&�@h�`@hbN@h �@gl�@f5?@e�@e�h@dj@cS�@b�@b��@b��@b��@b~�@b-@a��@a�@ahs@`�`@`Q�@_��@_|�@^��@^�@^��@^5?@]��@]?}@\��@\z�@\1@[dZ@Zn�@ZJ@Y�^@X��@Xr�@X  @W��@W��@V��@V��@V�+@VE�@U�@U��@U?}@T�/@Tj@S�
@S"�@R�H@R��@Rn�@RM�@Q��@Q�#@Q��@Qx�@QG�@Q7L@Q�@P��@P�`@P��@P �@O�;@O��@O�P@O|�@O+@N�y@N�+@M�-@M�@L�j@L�D@L�@Kƨ@K�@K33@Ko@K@J��@J�\@I��@IX@H�9@HbN@H1'@H  @G�@G�@GK�@F��@FE�@E�T@E��@E�@EO�@EO�@E/@E/@D�@DZ@D9X@D9X@D9X@C�
@C�F@C��@CS�@Co@C@B��@B�\@BM�@BJ@A��@A�7@Ax�@A7L@A%@@��@@Ĝ@@r�@@ �@?�w@?�P@?|�@?K�@?;d@>��@>��@>�+@>ff@>@=�T@=�-@=/@<��@<�@<j@<9X@<(�@;�
@;�@;@:��@:�!@:��@:~�@:M�@:J@9��@9��@9hs@9G�@8Ĝ@8�@8Q�@81'@8  @7�@7��@7K�@7+@6�@6�R@6�+@6v�@6V@6{@5@5�h@5p�@5V@4��@41@3ƨ@3dZ@2��@2�\@2M�@2=q@1�@1X@1�@0��@0��@0�`@0�u@0  @/�;@/�@/�P@/\)@/;d@/�@.�y@.E�@-��@-�-@-�h@-`B@,�@,(�@,�@,1@+�
@+33@+o@*��@*��@*n�@*^5@*�@)��@)�7@)hs@)&�@(�u@(A�@(1'@(1'@(b@'�;@'�@'l�@'K�@'�@&�@&ȴ@&�R@&�R@&��@&v�@&V@&5?@&{@&@%�@%��@%�@%�@$�@$��@$�j@$�@$z�@$I�@$9X@$9X@$9X@$�@#��@#�F@#dZ@#C�@#33@#@"��@"M�@!�@!��@!x�@ ��@ r�@  �@��@�@
=@�y@��@v�@V@$�@@�T@��@?}@/@�@�@�j@�@��@z�@I�@(�@�@��@��@S�@�@��@~�@n�@n�@^5@M�@J@��@��@��@�7@G�@�`@�@b@�@�;@��@|�@+@+@
=@��@ff@V@E�@5?@{@@��@@��@��@�h@`B@��@�D@j@Z@�@�F@��@�@C�@C�@"�@�@�H@��@n�@^5@-@J@�#@��@x�@X@7L@&�@�`@Ĝ@�u@�@bN@Q�@1'@  @�;@��@�@|�@l�@K�@+@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�5?A�$�A�+A� �A��A�bA�JA�A�%A�1A�A�  A���Aϴ9A��Ḁ�A��A���Aˏ\Aʡ�Aɝ�A�=qA�(�A���A�+AƶFAŝ�A�bNAģ�A�5?A�A�  A�|�A�-A��9A��A��+A�(�A��A�ȴA��A��7A�1'A��A���A�bNA�E�A� �A��A��!A�t�A�K�A�1A��^A�jA�G�A�/A�&�A�{A�A��A��wA�ffA�I�A�E�A��A���A�x�A���A�C�A�  A��A���A�K�A��mA�~�A�5?A��TA��9A�+A��-A��;A�ȴA�?}A���A�A�1A�bNA�=qA�-A�7LA���A�;dA�ZA�/A���A�C�A�I�A���A�E�A���A�K�A�"�A���A���A�VA���A�5?A���A�ƨA�bA��hA��7A���A�r�A�1'A��A���A�z�A��#A�=qA��A�x�A���A�jA��mA��A�ƨA�1'A��HA�wA}
=A{��AyVAx9XAw�Aw�AwAv��AqAo�wAodZAn�An1Al�Ah�HAe+Ac��Ac?}AaK�A]�A[�TA[/AZZAY;dAW�AU�FAQ?}AO�ANz�AM�;AK7LAH��AG�
AG+AF�RAE�AC�AA�
A?�mA>��A=��A<�/A;��A:z�A9K�A8��A7�7A6{A4n�A4  A1�A/�PA.ĜA.�A-�A,(�A*�\A'hsA%t�A$ZA#��A#A#XA!�A �!A�AjA%A��AjA��A��AJA�A��A�AK�A��A5?A�AAQ�A�TA;dAE�AoA
��A	l�A��AVAƨA"�A�9AƨA�`AM�A��A��A�\A~�A~�A��Ahs@�ƨ@�ff@��@���@�\)@���@���@��@���@��P@�V@��@�v�@��@�-@���@�^5@�x�@�`B@�-@�%@�D@�b@��@�~�@�hs@��@�$�@�^@�j@߶F@��m@��@ߕ�@�t�@�~�@�O�@�K�@ڇ+@�V@�
=@�=q@ա�@ա�@�p�@��@�bN@�K�@��@�X@��@���@�9X@��y@Ώ\@�hs@��/@̓u@̃@��;@˾w@��@��y@��@�7L@� �@�"�@Ƈ+@��@þw@�@+@�$�@��@��u@�Q�@��P@���@��\@�M�@��@��@���@�p�@��@���@�Z@�I�@�j@���@���@��H@��!@�=q@���@�9X@�|�@���@��!@�v�@�@�`B@���@��;@��@�dZ@�+@���@���@���@��+@�$�@���@��7@�X@��`@��u@�A�@� �@��m@���@�"�@���@�ȴ@���@�v�@��@�hs@���@���@��-@���@�z�@�9X@���@�l�@�@��@�bN@�r�@���@���@�dZ@��@�v�@�ȴ@��@���@��!@��+@�E�@���@��/@��/@���@��@��@�?}@��9@���@�bN@�9X@� �@��@�l�@�S�@�K�@�;d@�"�@�@��@���@�=q@��h@���@��u@�r�@�Z@�9X@� �@��m@�|�@�K�@���@�"�@�33@�C�@���@�J@��^@��T@��^@�x�@��u@�Q�@�I�@� �@��m@��
@��F@��w@��@�1'@�bN@�Z@�(�@��;@���@�33@���@��@���@�M�@�5?@���@���@�p�@�?}@�?}@�7L@��@�&�@��@��/@��D@�I�@� �@���@��F@�l�@���@���@�n�@�=q@��@�@��#@��^@���@�G�@�%@���@���@���@���@��@�A�@��;@�  @�ƨ@�;d@�+@�o@���@���@�ff@�=q@��@��7@�&�@��9@��@���@�K�@�o@�\)@�;d@�+@���@��@���@�=q@��#@��^@���@��^@�/@���@�Ĝ@��D@�I�@�1@��m@���@��@�l�@�+@��@�o@��@��!@��+@�V@��@���@�x�@�O�@�%@��@��@�j@�bN@�A�@�(�@�1@���@��m@�
=@��@���@�n�@��@��@���@���@���@��@��D@�1'@��@���@��F@���@�|�@�\)@��y@�V@��@��h@�hs@�/@�V@���@���@�9X@~��@~�@~�R@~ff@}�@}�@|�D@|(�@{��@{t�@z�H@zn�@zJ@yhs@xĜ@xbN@xQ�@xb@w�@v��@vE�@u�T@u�@t�@s�m@s33@r�!@r=q@q�^@q&�@p�`@p��@p1'@o��@ol�@o
=@n��@m�@m�@l��@lz�@l(�@k��@j�H@j=q@i�@i��@ihs@i&�@h�`@hbN@h �@gl�@f5?@e�@e�h@dj@cS�@b�@b��@b��@b��@b~�@b-@a��@a�@ahs@`�`@`Q�@_��@_|�@^��@^�@^��@^5?@]��@]?}@\��@\z�@\1@[dZ@Zn�@ZJ@Y�^@X��@Xr�@X  @W��@W��@V��@V��@V�+@VE�@U�@U��@U?}@T�/@Tj@S�
@S"�@R�H@R��@Rn�@RM�@Q��@Q�#@Q��@Qx�@QG�@Q7L@Q�@P��@P�`@P��@P �@O�;@O��@O�P@O|�@O+@N�y@N�+@M�-@M�@L�j@L�D@L�@Kƨ@K�@K33@Ko@K@J��@J�\@I��@IX@H�9@HbN@H1'@H  @G�@G�@GK�@F��@FE�@E�T@E��@E�@EO�@EO�@E/@E/@D�@DZ@D9X@D9X@D9X@C�
@C�F@C��@CS�@Co@C@B��@B�\@BM�@BJ@A��@A�7@Ax�@A7L@A%@@��@@Ĝ@@r�@@ �@?�w@?�P@?|�@?K�@?;d@>��@>��@>�+@>ff@>@=�T@=�-@=/@<��@<�@<j@<9X@<(�@;�
@;�@;@:��@:�!@:��@:~�@:M�@:J@9��@9��@9hs@9G�@8Ĝ@8�@8Q�@81'@8  @7�@7��@7K�@7+@6�@6�R@6�+@6v�@6V@6{@5@5�h@5p�@5V@4��@41@3ƨ@3dZ@2��@2�\@2M�@2=q@1�@1X@1�@0��@0��@0�`@0�u@0  @/�;@/�@/�P@/\)@/;d@/�@.�y@.E�@-��@-�-@-�h@-`B@,�@,(�@,�@,1@+�
@+33@+o@*��@*��@*n�@*^5@*�@)��@)�7@)hs@)&�@(�u@(A�@(1'@(1'@(b@'�;@'�@'l�@'K�@'�@&�@&ȴ@&�R@&�R@&��@&v�@&V@&5?@&{@&@%�@%��@%�@%�@$�@$��@$�j@$�@$z�@$I�@$9X@$9X@$9X@$�@#��@#�F@#dZ@#C�@#33@#@"��@"M�@!�@!��@!x�@ ��@ r�@  �@��@�@
=@�y@��@v�@V@$�@@�T@��@?}@/@�@�@�j@�@��@z�@I�@(�@�@��@��@S�@�@��@~�@n�@n�@^5@M�@J@��@��@��@�7@G�@�`@�@b@�@�;@��@|�@+@+@
=@��@ff@V@E�@5?@{@@��@@��@��@�h@`B@��@�D@j@Z@�@�F@��@�@C�@C�@"�@�@�H@��@n�@^5@-@J@�#@��@x�@X@7L@&�@�`@Ĝ@�u@�@bN@Q�@1'@  @�;@��@�@|�@l�@K�@+@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�yB�yB�yB�yB�yB�sB�sB�yB�yB�yB�yB�B�B�B��B�B��B��B��B�B8RB;dB=qBA�BS�BffBhsBjBq�Br�Bw�Bw�B~�B�B�B� B|�B{�B{�B{�B{�B|�B� B� B�B�7B�DB�VB�oB�uB�oB�oB�\B�7B�1B�%B�VB��B��B��B��B��B�B�!B�'B�3B�FB�^B�jB�jB�jB�jB��BÖBŢBɺB��B��B��B�B��B��B��B�jB�RB��B��B��B��B�BiyBs�B�uB�=Bq�BXB>wB.B)�B�BoBJB	7BB��B�TB��BB�B��B�bB|�Bo�BdZBT�BR�BP�BD�B:^B.B�B�B\B
��B
�TB
�B
ŢB
�!B
��B
��B
��B
�B
x�B
e`B
]/B
YB
S�B
P�B
O�B
)�B
�B
�B
uB

=B	��B	�`B	ɺB	�}B	�LB	��B	��B	�B	~�B	w�B	o�B	gmB	\)B	B�B	33B	)�B	$�B	�B	+B��B��B��B�B�sB�NB�B��B��B��BŢBÖB�jB�^B�FB�3B��B��B��B��B��B�{B�hB�PB�7B�B{�Bx�Bv�Bu�Bt�Br�Bn�Bl�BiyBgmBffBe`BdZBe`BffBhsBgmBiyBiyBm�Bm�Bo�Bm�Bl�Bk�Bm�BiyBiyBjBffBffBe`Be`BdZBe`BgmBffBiyBjBk�Bl�Bn�Bt�Bv�Bv�Bw�Bu�Bv�B{�B}�B|�B}�B�B�B�+B�=B�B�+B�1B�%B�B�B�hB��B��B��B��B��B��B��B��B��B�'B�LB�?B�LB�jBĜBǮBǮB��B��B��B��B��B�B��B�B�BB�fB�mB�sB�fB�mB�B�B��B��B��B��B	B	B	1B	
=B	JB	bB	bB	hB	{B	hB	bB	PB	JB	JB	PB	JB	DB	PB	bB	hB	hB	uB	{B	�B	�B	�B	�B	�B	"�B	#�B	'�B	)�B	.B	6FB	<jB	:^B	<jB	=qB	>wB	?}B	>wB	=qB	@�B	B�B	A�B	@�B	?}B	C�B	J�B	L�B	O�B	P�B	Q�B	R�B	S�B	T�B	VB	VB	W
B	XB	ZB	[#B	]/B	`BB	bNB	e`B	gmB	gmB	iyB	iyB	hsB	iyB	jB	m�B	q�B	u�B	v�B	w�B	}�B	~�B	� B	�B	�B	�B	�%B	�=B	�=B	�=B	�7B	�1B	�JB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�-B	�9B	�?B	�?B	�?B	�LB	�RB	�^B	�jB	�jB	�qB	�wB	�}B	��B	ĜB	ǮB	ȴB	ǮB	ǮB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�)B	�;B	�HB	�TB	�TB	�ZB	�`B	�fB	�fB	�fB	�mB	�mB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
  B
B
  B
  B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
	7B
DB

=B
DB
DB
JB
JB
JB
JB
PB
VB
VB
VB
bB
bB
bB
bB
bB
bB
hB
hB
oB
{B
{B
�B
�B
�B
{B
{B
{B
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
!�B
!�B
!�B
"�B
"�B
"�B
#�B
$�B
$�B
$�B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
(�B
)�B
)�B
)�B
)�B
+B
,B
.B
.B
.B
.B
.B
.B
/B
/B
/B
1'B
2-B
2-B
33B
49B
5?B
5?B
5?B
5?B
5?B
6FB
6FB
6FB
6FB
7LB
7LB
8RB
8RB
8RB
8RB
8RB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
;dB
;dB
;dB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
=qB
=qB
=qB
=qB
>wB
>wB
?}B
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
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
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
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
J�B
J�B
J�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
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
VB
VB
VB
VB
VB
VB
VB
VB
W
B
W
B
W
B
XB
XB
XB
XB
YB
YB
YB
YB
ZB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
\)B
\)B
\)B
\)B
]/B
]/B
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
cTB
cTB
bNB
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
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
ffB
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
iyB
iyB
iyB
iyB
iyB
iyB
jB
jB
jB
jB
jB
jB
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
n�B
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
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
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
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
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
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�B�yB�B�B�yB�B�B�B�B�B��B��B��B��B�RB�;B�VB��B��BjB8�B;�B>]BC�BV�Bh$Bi_Bl"Bs3Bu�By	Bx�B�B�'B��B��B}�B|jB|6B|PB|jB}�B��B��B��B��B��B��B��B��B��B�B�B��B��B��B�pB��B��B�B�bB��B�cB�UB��B��B�2B��B��B��B��B�<B�[BĜBƨBʌBϫBЗB�FB�KB��B��B��B�B�*B��B��B�vB��B��Bj�ButB�gB�Bt�B[�B@�B0!B,�B�B@BB
�B%B�8B�BרB��B��B��B��B}Bq�Bf�BVBTFBS�BG�B>B1'B�B�B&B
�B
�B
��B
�fB
��B
�RB
�TB
�eB
�-B
{JB
f�B
]�B
Y�B
T�B
R�B
T�B
+kB
]B
yB
B
�B
UB	�DB	˒B	� B	�DB	��B	�sB	�tB	�OB	y�B	q�B	j�B	`�B	D�B	4�B	+�B	'�B	B	fB	  B��B��B�+B�6B�B��B�,B�HB�VB�zB�B�qB�jB�lB�?B�kB�B��B��B��B�B�@B��B��B�{B}<ByrBw�Bv�Bv�Bt�BqBo�BkBh$Bf�BfLBe�Bf�Bh$BjBi*BkBjKBncBncBp�Bn�BmwBl�Bo5Bk�BlWBlBgmBg8BfLBfLBeFBf�Bh�BgmBjeBk�BlBl�Bo Bu�Bw�Bx�Bx�Bv`Bw�B}B~�B}qB~�B��B��B��B�0B��B��B�	B��B��B��B�hB��B�=B�B�IB��B�BB��B�_B��B��B�B��B�fB��B��B�BȀB��B��B̘B�B�9BևBՁB�EB��B��B�
B�_B�RB�
B� B�B�`B��B�}B��B	uB	mB	fB	
�B	�B	�B	�B	�B	�B	TB	4B	"B	jB	PB	�B	�B	�B	"B	�B	�B	 B	�B	�B	�B	�B	�B	�B	�B	#:B	$@B	($B	*B	./B	6`B	=<B	:�B	<�B	>B	?cB	@ B	?B	=�B	@�B	B�B	A�B	A B	@B	DMB	J�B	MB	P.B	Q4B	R B	SB	TFB	UgB	VmB	VSB	WYB	XyB	Z�B	[qB	]~B	`�B	b�B	e�B	g�B	g�B	i�B	i�B	h�B	i�B	j�B	m�B	q�B	v`B	v�B	xB	~wB	cB	��B	��B	��B	�GB	�?B	��B	��B	��B	��B	�B	�JB	��B	��B	��B	�	B	��B	��B	��B	��B	��B	��B	�cB	��B	�aB	�nB	�ZB	�tB	��B	��B	��B	�xB	��B	��B	��B	��B	��B	�B	�B	�B	��B	��B	��B	��B	��B	�	B	�)B	��B	��B	��B	��B	��B	�FB	�aB	� B	��B	�,B	�gB	�{B	�:B	� B	�&B	�B	�&B	�&B	��B	��B	�B	�)B	�VB	�|B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	��B	�B	��B	��B	�B	��B	��B	��B	�B	��B	�B	�B	�	B	�>B	�0B	�B	�B	�"B	�B	�B	�.B	�HB
 4B
 OB
 B
  B
  B
 4B
UB
 OB
 OB	��B
;B
[B
AB
'B
AB
[B
[B
aB
{B
aB
�B
�B
�B
AB
[B
GB
B
	lB
xB

�B
xB
�B
�B
~B
dB
JB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
$B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
�B
#B
/B
�B
�B
B
B
�B
B
�B
�B
 B
 B
�B
 �B
!B
!B
 �B
 �B
 �B
!�B
"4B
"4B
# B
#B
# B
$&B
%B
%B
%B
&B
&2B
'B
'B
'8B
'B
'8B
(>B
(>B
(XB
)DB
*0B
*0B
*KB
*0B
+QB
,=B
./B
.IB
.IB
.IB
.IB
.IB
/OB
/iB
/�B
1AB
2|B
2�B
3�B
4TB
5ZB
5ZB
5tB
5ZB
5ZB
6zB
6zB
6�B
6zB
7�B
7�B
8�B
8�B
8lB
8�B
8�B
9�B
9�B
9�B
9�B
:�B
:�B
:�B
;�B
;�B
;�B
<�B
<�B
<�B
<�B
<�B
<�B
<�B
<�B
=�B
=�B
=�B
=�B
>�B
>�B
?�B
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
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
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
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
J�B
J�B
J�B
K�B
K�B
MB
L�B
MB
MB
MB
L�B
NB
M�B
NB
NB
M�B
M�B
NB
N�B
OB
OB
N�B
OB
OB
O(B
O�B
PB
O�B
PB
PB
O�B
QB
Q4B
R B
R B
RB
R B
R B
R B
SB
SB
SB
SB
S@B
TB
T,B
T,B
TB
U2B
UB
UB
U2B
U2B
VB
VB
VB
V9B
VB
VB
V9B
V9B
WYB
WYB
W?B
X+B
XEB
X_B
X+B
YKB
Y1B
YKB
YeB
ZQB
ZQB
Z7B
ZQB
Z7B
ZkB
[WB
[=B
[=B
[WB
[WB
[WB
[=B
[WB
\CB
\CB
\]B
\]B
]~B
]~B
^5B
^jB
^OB
^jB
_VB
_pB
_pB
_pB
_pB
_VB
`vB
`\B
`\B
`\B
`vB
abB
abB
aHB
abB
a|B
a|B
a|B
bhB
b�B
b�B
bhB
cnB
cTB
bhB
cnB
c�B
c�B
cnB
cnB
cnB
c�B
c�B
dtB
d�B
d�B
dtB
d�B
dtB
e�B
ezB
ezB
e`B
ezB
e�B
e�B
e�B
f�B
f�B
f�B
f�B
f�B
f�B
g�B
g�B
g�B
g�B
h�B
h�B
i�B
i�B
i�B
i�B
i�B
i�B
j�B
j�B
j�B
j�B
j�B
j�B
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
n�B
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
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
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
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
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
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.11(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201704230035012017042300350120170423003501201806221312162018062213121620180622131216201804050713322018040507133220180405071332  JA  ARFMdecpA19c                                                                20170419093510  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20170419003539  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20170419003540  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20170419003541  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20170419003541  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20170419003541  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20170419003541  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20170419003541  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20170419003542  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20170419003542                      G�O�G�O�G�O�                JA  ARUP                                                                        20170419010801                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20170419153504  CV  JULD            G�O�G�O�F��                JM  ARCAJMQC2.0                                                                 20170422153501  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20170422153501  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404221332  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622041216  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116211515                      G�O�G�O�G�O�                