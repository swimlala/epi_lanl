CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-01-08T00:35:17Z creation;2018-01-08T00:35:21Z conversion to V3.1;2019-12-19T07:52:21Z update;     
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
_FillValue                 �  I<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  pL   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t4   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �\   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �l   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ݬ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �<   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �<   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �<   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �<   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �    HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �0   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �4   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �8   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �<Argo profile    3.1 1.2 19500101000000  20180108003517  20200115121516  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0576_197                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�B�3�J�1   @�B���� @;<�_���dT����1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� Dz��D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ Dļ�D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�<�DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D��3D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�3D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�P 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@x��@�z�@�z�A=qA>=qA^=qA~=qA��A��A��A��A��A��A��A��B�\B�\B�\B�\B'�\B/�\B7�\B?�\BG�\BO�\BW�\B_�\Bg�\Bo�\Bw�\B�\B�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮC��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D x�D ��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��D	x�D	��D
x�D
��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��D x�D ��D!x�D!��D"x�D"��D#x�D#��D$x�D$��D%x�D%��D&x�D&��D'x�D'��D(x�D(��D)x�D)��D*x�D*��D+x�D+��D,x�D,��D-x�D-��D.x�D.��D/x�D/��D0x�D0��D1x�D1��D2x�D2��D3x�D3��D4x�D4��D5x�D5��D6x�D6��D7x�D7��D8x�D8��D9x�D9��D:x�D:��D;x�D;��D<x�D<��D=x�D=��D>x�D>��D?x�D?��D@x�D@��DAx�DA��DBx�DB��DCx�DC��DDx�DD��DEx�DE��DFx�DF��DGx�DG��DHx�DH��DIx�DI��DJx�DJ��DKx�DK��DLx�DL��DMx�DM��DNx�DN��DOx�DO��DPx�DP��DQx�DQ��DRx�DR��DSx�DS��DTx�DT��DUx�DU��DVx�DV��DWx�DW��DXx�DX��DYx�DY��DZx�DZ��D[x�D[��D\x�D\��D]x�D]��D^x�D^��D_x�D_��D`x�D`��Dax�Da��Dbx�Db��Dcx�Dc��Ddx�Dd��Dex�De��Dfx�Df��Dgx�Dg��Dhx�Dh��Dix�Di��Djx�Dj��Dkx�Dk��Dlx�Dl��Dmx�Dm��Dnx�Dn��Dox�Do��Dpx�Dp��Dqx�Dq��Drx�Dr��Dsx�Ds��Dtx�Dt��Dux�Du��Dvx�Dv��Dwx�Dw��Dxx�Dx��Dyx�Dy��Dzx�Dz�D{x�D{��D|x�D|��D}x�D}��D~x�D~��Dx�D��D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�9HD�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D¼{D��{D�<{D�|{Dü{D��{D�<{D�|{DĹHD��{D�<{D�|{Dż{D��{D�<{D�|{DƼ{D��{D�<{D�|{DǼ{D��{D�<{D�|{Dȼ{D��{D�<{D�|{Dɼ{D��{D�<{D�|{Dʼ{D��{D�<{D�|{D˼{D��{D�<{D�|{D̼{D��{D�<{D�|{Dͼ{D��{D�<{D�|{Dμ{D��{D�<{D�|{Dϼ{D��{D�<{D�|{Dм{D��{D�<{D�|{DѼ{D��{D�<{D�|{DҼ{D��{D�<{D�|{DӼ{D��{D�<{D�|{DԼ{D��{D�9HD�|{Dռ{D��{D�<{D�|{Dּ{D��{D�<{D�|{D׿�D��{D�<{D�|{Dؼ{D��{D�<{D�|{Dټ{D���D�<{D�|{Dڼ{D��{D�<{D�|{Dۼ{D��{D�<{D�|{Dܼ{D��{D�<{D�|{Dݼ{D��{D�<{D�|{D޼{D��{D�<{D�|{D߼{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D��{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D���D�L{1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A���A���A���A���A���A���A���A���A��FA��hA�~�A�r�A�hsA�dZA�^5A�ZA�VA�VA�I�A�?}A�+A��A��A�{A�1A��A���A���A�p�A�Q�A�1'A�1A��A���A���A���A��uA��\A��uA���A��uA��hA��hA��\A�z�A��A��A���A��A�ƨA��A�JA�A�A��RA�O�A�`BA�C�A�5?A���A�bNA�-A�v�A��A�S�A���A��
A��uA���A�9XA���A�ffA�7LA��
A��A�ZA��!A�ȴA~�!A|-A{C�AzVAz(�Ay��AydZAxn�Aw�wAwAvI�Au�
Au��Au7LAt�jAt�!AtQ�Ap��AoG�An�!Am�PAlVAkC�Ak"�Aj�AjffAi�Ag�
Af�yAfz�Ae��Ae�7Ad�`AdZAc�Ac�7Ab�yAbbNAaƨAaA_+A\JAZ9XAY�AX^5AWAV$�AU&�AS7LARr�ARv�ARn�AQ�AQG�AP��AP�\APAM%AK33AJ��AJ�AI�mAI�mAI��AIK�AH��AH�AH�AH�/AH�!AHz�AHbNAH{AHAG��AG&�AFz�AE�AD�AC�-AB-AA�-AA%A@�`A@��A@��A>�\A=p�A<�`A< �A;��A:A7�;A6bA5�A4��A4~�A3��A3&�A3%A1�7A0-A/l�A.�A.M�A-��A-x�A,��A,~�A+�;A*ffA)oA(=qA'A&5?A%�TA%�hA%VA$��A#\)A"Q�A!�A!7LA �/A E�A�#A��Ap�A�A�A��A�\A{A��A�wA��A�Av�A^5AE�A�TA��A
=A~�A`BA��A��A��A�DA�A7LA�A��A9XAƨAE�A�A7LA��AƨA
�A	l�A�TA��A-A1A��A�`A?}A�A ��@��h@��;@�M�@�hs@�r�@�+@���@��@��@�S�@�-@홚@��m@�?}@��@���@�&�@�\)@�R@�\@�-@�p�@���@޸R@�hs@�9X@ە�@�$�@�r�@�
=@�~�@��#@���@�ƨ@Ӆ@�;d@�5?@�O�@�C�@��@��#@���@�A�@���@Õ�@�\)@��@�ȴ@\@�V@�7L@�j@��
@��@�ff@�J@��T@��7@�j@��m@��;@��
@���@���@�ƨ@��w@���@�dZ@��@�n�@�l�@�1'@��R@���@�O�@��@�V@��@��`@�z�@���@��^@�p�@�hs@�O�@�?}@�&�@�I�@��y@�-@���@�G�@���@�Ĝ@��@�1'@�  @��w@�dZ@�;d@�"�@���@�E�@��@��@�A�@���@���@��F@���@�|�@�"�@��+@��#@�O�@��/@�Ĝ@��@�bN@��m@�l�@���@�-@��#@���@�%@�r�@�Q�@��F@���@�l�@���@��^@��D@�  @�o@�M�@��-@��@�?}@�7L@��`@�z�@�I�@���@�+@��T@�`B@�7L@���@�(�@�|�@�
=@���@��T@�X@�z�@�9X@��@���@��@���@�t�@���@�ff@�5?@�@���@��@�hs@�X@�O�@�?}@�/@��@���@��@�33@��@�~�@�M�@�=q@�@���@���@�p�@�X@�/@�%@��/@�z�@�1'@��;@��@���@���@��@�33@���@�V@��@���@�`B@�G�@���@�Z@�1'@�@~��@}�T@}�h@}`B@}V@|�/@|�D@|(�@|�@{��@{�
@{�
@{�
@{�
@{�F@{@y�@yhs@y�@x�9@x�@x��@x�`@y�@yX@xr�@w�P@wK�@vff@u`B@t�j@tz�@t(�@st�@sC�@so@r�!@q�@q��@q%@pbN@o��@o;d@n�R@n�+@nff@nv�@nff@nE�@m��@m��@n@m�T@m�@n$�@n{@m�h@m/@l��@l�@l9X@k��@k�m@kƨ@kS�@kC�@k33@k"�@k"�@ko@k@ko@j�H@j��@j�\@j=q@j=q@j=q@j=q@j�@i��@ix�@h�`@h��@g�w@f��@f�@f�R@f�+@f�+@f�+@f�+@fv�@fV@e@eO�@eV@d�j@dj@d(�@cƨ@c"�@bn�@b^5@bM�@b-@bJ@a��@a�@a��@ax�@a&�@`�`@`Ĝ@` �@_�@_�@^�R@^v�@^@]p�@]/@]�@\��@\�@\I�@\9X@\�@[��@[�m@[�
@[dZ@Z�H@Z�!@Y�@X��@X�u@X�@X  @V�@VE�@U�h@U`B@UO�@U/@T��@T�@T��@T��@Tj@T9X@T�@T1@S��@S�F@S��@St�@SdZ@SC�@R�H@Q&�@O�@O��@O�@O��@N�@N@M@M�h@MO�@L�D@L�@K��@K�@K"�@J��@Jn�@J=q@Ix�@I&�@HbN@G�@G�w@G|�@G+@G
=@F�@Fȴ@F�R@F��@F�+@FV@E@Ep�@EV@D�j@Dj@C�
@C@Bn�@A�7@A�@@Ĝ@@�@@b@?�;@?�@?l�@?�@>��@>�y@>ff@=`B@=/@=V@<�@<�/@<�j@<��@<Z@;��@;�F@;dZ@:�@:��@:^5@:�@9��@9&�@8�@8Q�@8 �@7��@7�P@7\)@7+@6�@6��@6v�@6$�@6{@6{@6@5�T@5��@5�-@5�-@5��@5�h@5�@5p�@5�@4�j@4z�@4�@3��@3S�@333@3o@2�\@2J@1�@1�^@1hs@1&�@1&�@1�@0��@0��@0Ĝ@0��@0�u@0r�@0bN@0 �@/�w@/\)@/
=@.�R@.��@.v�@.ff@.ff@.V@.V@.5?@.$�@.$�@.@-�T@-@-�h@-/@,��@,��@,z�@,j@,9X@+��@+ƨ@+t�@+@*��@*^5@*J@)��@)�7@(�`@(bN@(Q�@(Q�@(bN@(bN@(Q�@'�@'��@'��@'��@'�P@'�P@'�P@'|�@'l�@'K�@';d@';d@&��@&��@&$�@%��@%��@%��@%��@%`B@%O�@%?}@%V@$�@$�@$��@$�D@$�D@$j@#�
@#�F@#��@#��@#��@#�@#dZ@#dZ@#C�@#"�@#"�@"�H@"��@"n�@"M�@!��@!x�@!%@ Ĝ@ 1'@��@�w@\)@��@�@��@V@$�@{@@�T@p�@�@��@�j@��@�
@ƨ@��@t�@"�@�H@�\@^5@=q@�@�#@�7@hs@G�@7L@&�@��@�@bN@A�@  @��@��@K�@�@E�@�T@�-@�@/@/@V@��@�@�D@z�@j@Z@9X@1@�
@�F@��@t�@"�@~�@�@�@�#@��@�^@��@�7@hs@G�@&�@�@��@�`@��@bN@ �@b@�;@�@l�@+@��@��@ff@V@E�@$�@�T@��@�h@�@p�@/@�@��@z�@j@j@j@(�@�@33@
�@
�\@
=q@
J@	��@	�@	x�@	hs@	7L@	�@�`@Ĝ@��@��@�u@�@r�@r�@bN@Q�@A�@ �@�;@�w@��@l�@��@$�@{@@�T@��@��@��@p�@O�@/@��@�/@��@j@I�@1@�m@�
@ƨ@�F@�F@�F@�F@��@��@t�@S�@C�@33@o@�@��@�!@��@^5@=q@��@��@��@��@��@��@x�@G�@&�@�@�@�@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A���A���A���A���A���A���A���A���A��FA��hA�~�A�r�A�hsA�dZA�^5A�ZA�VA�VA�I�A�?}A�+A��A��A�{A�1A��A���A���A�p�A�Q�A�1'A�1A��A���A���A���A��uA��\A��uA���A��uA��hA��hA��\A�z�A��A��A���A��A�ƨA��A�JA�A�A��RA�O�A�`BA�C�A�5?A���A�bNA�-A�v�A��A�S�A���A��
A��uA���A�9XA���A�ffA�7LA��
A��A�ZA��!A�ȴA~�!A|-A{C�AzVAz(�Ay��AydZAxn�Aw�wAwAvI�Au�
Au��Au7LAt�jAt�!AtQ�Ap��AoG�An�!Am�PAlVAkC�Ak"�Aj�AjffAi�Ag�
Af�yAfz�Ae��Ae�7Ad�`AdZAc�Ac�7Ab�yAbbNAaƨAaA_+A\JAZ9XAY�AX^5AWAV$�AU&�AS7LARr�ARv�ARn�AQ�AQG�AP��AP�\APAM%AK33AJ��AJ�AI�mAI�mAI��AIK�AH��AH�AH�AH�/AH�!AHz�AHbNAH{AHAG��AG&�AFz�AE�AD�AC�-AB-AA�-AA%A@�`A@��A@��A>�\A=p�A<�`A< �A;��A:A7�;A6bA5�A4��A4~�A3��A3&�A3%A1�7A0-A/l�A.�A.M�A-��A-x�A,��A,~�A+�;A*ffA)oA(=qA'A&5?A%�TA%�hA%VA$��A#\)A"Q�A!�A!7LA �/A E�A�#A��Ap�A�A�A��A�\A{A��A�wA��A�Av�A^5AE�A�TA��A
=A~�A`BA��A��A��A�DA�A7LA�A��A9XAƨAE�A�A7LA��AƨA
�A	l�A�TA��A-A1A��A�`A?}A�A ��@��h@��;@�M�@�hs@�r�@�+@���@��@��@�S�@�-@홚@��m@�?}@��@���@�&�@�\)@�R@�\@�-@�p�@���@޸R@�hs@�9X@ە�@�$�@�r�@�
=@�~�@��#@���@�ƨ@Ӆ@�;d@�5?@�O�@�C�@��@��#@���@�A�@���@Õ�@�\)@��@�ȴ@\@�V@�7L@�j@��
@��@�ff@�J@��T@��7@�j@��m@��;@��
@���@���@�ƨ@��w@���@�dZ@��@�n�@�l�@�1'@��R@���@�O�@��@�V@��@��`@�z�@���@��^@�p�@�hs@�O�@�?}@�&�@�I�@��y@�-@���@�G�@���@�Ĝ@��@�1'@�  @��w@�dZ@�;d@�"�@���@�E�@��@��@�A�@���@���@��F@���@�|�@�"�@��+@��#@�O�@��/@�Ĝ@��@�bN@��m@�l�@���@�-@��#@���@�%@�r�@�Q�@��F@���@�l�@���@��^@��D@�  @�o@�M�@��-@��@�?}@�7L@��`@�z�@�I�@���@�+@��T@�`B@�7L@���@�(�@�|�@�
=@���@��T@�X@�z�@�9X@��@���@��@���@�t�@���@�ff@�5?@�@���@��@�hs@�X@�O�@�?}@�/@��@���@��@�33@��@�~�@�M�@�=q@�@���@���@�p�@�X@�/@�%@��/@�z�@�1'@��;@��@���@���@��@�33@���@�V@��@���@�`B@�G�@���@�Z@�1'@�@~��@}�T@}�h@}`B@}V@|�/@|�D@|(�@|�@{��@{�
@{�
@{�
@{�
@{�F@{@y�@yhs@y�@x�9@x�@x��@x�`@y�@yX@xr�@w�P@wK�@vff@u`B@t�j@tz�@t(�@st�@sC�@so@r�!@q�@q��@q%@pbN@o��@o;d@n�R@n�+@nff@nv�@nff@nE�@m��@m��@n@m�T@m�@n$�@n{@m�h@m/@l��@l�@l9X@k��@k�m@kƨ@kS�@kC�@k33@k"�@k"�@ko@k@ko@j�H@j��@j�\@j=q@j=q@j=q@j=q@j�@i��@ix�@h�`@h��@g�w@f��@f�@f�R@f�+@f�+@f�+@f�+@fv�@fV@e@eO�@eV@d�j@dj@d(�@cƨ@c"�@bn�@b^5@bM�@b-@bJ@a��@a�@a��@ax�@a&�@`�`@`Ĝ@` �@_�@_�@^�R@^v�@^@]p�@]/@]�@\��@\�@\I�@\9X@\�@[��@[�m@[�
@[dZ@Z�H@Z�!@Y�@X��@X�u@X�@X  @V�@VE�@U�h@U`B@UO�@U/@T��@T�@T��@T��@Tj@T9X@T�@T1@S��@S�F@S��@St�@SdZ@SC�@R�H@Q&�@O�@O��@O�@O��@N�@N@M@M�h@MO�@L�D@L�@K��@K�@K"�@J��@Jn�@J=q@Ix�@I&�@HbN@G�@G�w@G|�@G+@G
=@F�@Fȴ@F�R@F��@F�+@FV@E@Ep�@EV@D�j@Dj@C�
@C@Bn�@A�7@A�@@Ĝ@@�@@b@?�;@?�@?l�@?�@>��@>�y@>ff@=`B@=/@=V@<�@<�/@<�j@<��@<Z@;��@;�F@;dZ@:�@:��@:^5@:�@9��@9&�@8�@8Q�@8 �@7��@7�P@7\)@7+@6�@6��@6v�@6$�@6{@6{@6@5�T@5��@5�-@5�-@5��@5�h@5�@5p�@5�@4�j@4z�@4�@3��@3S�@333@3o@2�\@2J@1�@1�^@1hs@1&�@1&�@1�@0��@0��@0Ĝ@0��@0�u@0r�@0bN@0 �@/�w@/\)@/
=@.�R@.��@.v�@.ff@.ff@.V@.V@.5?@.$�@.$�@.@-�T@-@-�h@-/@,��@,��@,z�@,j@,9X@+��@+ƨ@+t�@+@*��@*^5@*J@)��@)�7@(�`@(bN@(Q�@(Q�@(bN@(bN@(Q�@'�@'��@'��@'��@'�P@'�P@'�P@'|�@'l�@'K�@';d@';d@&��@&��@&$�@%��@%��@%��@%��@%`B@%O�@%?}@%V@$�@$�@$��@$�D@$�D@$j@#�
@#�F@#��@#��@#��@#�@#dZ@#dZ@#C�@#"�@#"�@"�H@"��@"n�@"M�@!��@!x�@!%@ Ĝ@ 1'@��@�w@\)@��@�@��@V@$�@{@@�T@p�@�@��@�j@��@�
@ƨ@��@t�@"�@�H@�\@^5@=q@�@�#@�7@hs@G�@7L@&�@��@�@bN@A�@  @��@��@K�@�@E�@�T@�-@�@/@/@V@��@�@�D@z�@j@Z@9X@1@�
@�F@��@t�@"�@~�@�@�@�#@��@�^@��@�7@hs@G�@&�@�@��@�`@��@bN@ �@b@�;@�@l�@+@��@��@ff@V@E�@$�@�T@��@�h@�@p�@/@�@��@z�@j@j@j@(�@�@33@
�@
�\@
=q@
J@	��@	�@	x�@	hs@	7L@	�@�`@Ĝ@��@��@�u@�@r�@r�@bN@Q�@A�@ �@�;@�w@��@l�@��@$�@{@@�T@��@��@��@p�@O�@/@��@�/@��@j@I�@1@�m@�
@ƨ@�F@�F@�F@�F@��@��@t�@S�@C�@33@o@�@��@�!@��@^5@=q@��@��@��@��@��@��@x�@G�@&�@�@�@�@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B �B �B �B �B�B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�BuBhB\BVBPBPBhBoBhBbBbB\BbB\B\B\BVBPBDB%B��B��B�B�wB�9B�RBl�B��B�uB�1Bz�BdZB\)BL�B@�B49B$�BVB
��B
��B
�`B
�B
��B
��B
ɺB
ǮB
B
�wB
�XB
�!B
��B
�7B
x�B
s�B
o�B
m�B
l�B
hsB
bNB
`BB
\)B
XB
VB
S�B
Q�B
L�B
J�B
D�B
49B
)�B
$�B
�B
�B
oB
hB
\B
DB
B	��B	��B	�B	�B	�B	�B	�mB	�ZB	�HB	�/B	�B	��B	��B	��B	�!B	��B	��B	��B	��B	�\B	�=B	�B	~�B	�B	� B	{�B	w�B	t�B	r�B	jB	[#B	S�B	W
B	S�B	S�B	T�B	R�B	P�B	N�B	P�B	P�B	O�B	N�B	L�B	L�B	J�B	I�B	G�B	C�B	@�B	;dB	5?B	.B	(�B	(�B	%�B	%�B	$�B	�B	�B	VB	PB	1B	B��B�B�NB�NB�BB�/B�B�B�B��BɺBɺBȴBƨBƨBÖB��B�wB�^B�3B�'B�3B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�uB�uB�oB�\B�PB�VB�VB�JB�=B�+B�B}�Bz�Bw�Br�Bq�Bl�B`BBVB_;B]/BZBVBT�BXBT�BO�BI�BI�BD�B@�B@�BA�B>wB9XB33B2-B/B(�B.B-B.B-B+B(�B%�B �B"�B%�B$�B!�B�B!�B#�B �B!�B%�B&�B&�B'�B0!B33B49B5?B5?B2-B1'B2-B5?B49B2-B2-B2-B2-B/B,B'�B&�B�B"�B'�B/B0!B1'B1'B1'B1'B1'B/B1'B7LB:^B=qB?}B?}B>wB<jB?}BB�BB�BB�BB�BB�BA�BA�B@�B>wB<jB49B6FB:^B=qBB�BH�BI�BI�BH�BG�BD�BF�BN�BT�BW
BZB]/B`BBe`BjBo�Bo�Bq�Bs�Bt�Bt�Bv�Bv�Bw�By�Bz�By�B{�B~�B~�B�B�+B�1B�1B�1B�1B�+B�1B�7B�=B�DB�PB�VB�PB�VB�hB�hB�{B�{B��B��B��B��B��B��B��B��B��B��B�B�B�9B�^B�}BBĜBĜBŢBɺB��B��B��B�
B�B�B�B�/B�NB�fB�mB�B�B�B��B��B��B��B��B��B��B��B��B��B��B	B	B	B	B	B	B	B	  B��B	  B	B	B	%B	%B	%B	+B		7B	
=B	DB	JB	PB	PB	hB	{B	�B	�B	�B	�B	�B	�B	�B	�B	#�B	&�B	)�B	)�B	/B	1'B	2-B	49B	:^B	=qB	>wB	?}B	A�B	B�B	C�B	F�B	G�B	H�B	H�B	I�B	H�B	H�B	G�B	H�B	L�B	O�B	R�B	VB	ZB	^5B	aHB	e`B	jB	o�B	q�B	q�B	q�B	r�B	t�B	t�B	u�B	w�B	w�B	x�B	y�B	|�B	|�B	� B	�B	�B	�7B	�PB	�\B	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�3B	�9B	�9B	�?B	�FB	�FB	�RB	�jB	�}B	�}B	��B	B	ĜB	ŢB	ŢB	ŢB	ƨB	ƨB	ƨB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�#B	�#B	�#B	�#B	�)B	�)B	�)B	�/B	�5B	�5B	�5B	�BB	�BB	�NB	�TB	�TB	�ZB	�`B	�mB	�mB	�fB	�mB	�sB	�sB	�sB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
  B
  B
  B
B
B
B
B
B
B
+B
+B
1B
1B
	7B
	7B
DB
DB
DB
PB
PB
VB
\B
\B
bB
bB
bB
bB
bB
\B
hB
hB
oB
oB
oB
oB
uB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
#�B
#�B
#�B
$�B
$�B
$�B
%�B
&�B
&�B
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
+B
+B
,B
-B
-B
.B
/B
/B
/B
/B
/B
0!B
1'B
1'B
2-B
2-B
33B
33B
49B
49B
49B
49B
49B
49B
33B
33B
33B
49B
5?B
6FB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
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
:^B
:^B
:^B
9XB
9XB
9XB
;dB
;dB
<jB
;dB
;dB
:^B
<jB
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
>wB
>wB
?}B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
B�B
C�B
C�B
D�B
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
F�B
F�B
G�B
G�B
G�B
H�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
J�B
K�B
K�B
K�B
K�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
Q�B
S�B
T�B
VB
T�B
W
B
VB
VB
W
B
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
XB
XB
XB
YB
ZB
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
\)B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
_;B
_;B
`BB
_;B
_;B
_;B
_;B
_;B
`BB
_;B
_;B
`BB
aHB
aHB
aHB
aHB
`BB
`BB
aHB
bNB
bNB
cTB
cTB
dZB
cTB
cTB
dZB
e`B
e`B
e`B
ffB
ffB
ffB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
ffB
iyB
iyB
iyB
iyB
iyB
jB
iyB
jB
jB
jB
jB
jB
k�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
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
n�B
o�B
o�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B �B �B �B �B�B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B}B\BbBvBvB\BpB�B�B+B��B��B�/B� B�B�iG�O�B̳B��B��B}�Bf�B^BO�BCGB6zB(XB�B
�]B
�LB
�>B
�CB
�B
�B
�rB
�fB
ÖB
�}B
�^B
��B
�B
�~B
{�B
t�B
p�B
m�B
mB
i_B
cnB
aHB
]B
X�B
V�B
TFB
RoB
M�B
K^B
FB
7�B
+�B
%�B
!-B
B
�B
�B
�B
0B
�B	��B	��B	�TB	�[B	�OB	�kB	�$B	��B	��B	�B	��B	�B	�pB	�3B	��B	�
B	�B	��B	��B	�NB	��B	�3B	�B	� B	�OB	|�B	x�B	u�B	shB	l"B	^�B	VB	W�B	T�B	TFB	U2B	SuB	QhB	O(B	Q B	Q B	P.B	OBB	MB	MB	KDB	J#B	HKB	DMB	A�B	<jB	6�B	/�B	*�B	)�B	&�B	&LB	%`B	 �B	�B	�B	"B		RB	3B�6B�B�tB�:B�-B��B�B��BּB��B�^B��B�lBǔB�EB�MB�oB�cB��B�?B��B�nB��B�B��B��B��B��B�|B�B��B��B�]B��B�KB�B��B�B��B��B�B�,B��B��B��B�B�B��B��B��B��B�1B�3B}B|Bx�BtBr�Bm�Bc�BXyB_�B^5B[=BW�BVBX�BVBQhBK�BKxBF�BBBAUBA�B?cB:�B5tB4B1'B+�B/OB./B.�B-�B,B*0B'�B"�B$B&�B%�B#TB vB"�B$�B"4B#B&fB'8B'RB(�B0�B4�B5?B6B5�B3hB2|B3B5�B4�B3B2�B2�B2�B/�B-)B)yB(sB�B%FB(�B/�B0oB1vB1�B1�B1vB1�B0B1�B7�B;B=�B?�B?�B>�B=VB?�BB�BB�BB�BB�BB�BA�BA�B@�B>�B=�B6zB8lB;dB>BB�BIBI�BJ	BIBHKBE�BG�BOBU2BW?BZkB]�BaBfLBkBo�Bp;BrBtBuBu%Bv�Bw2Bx8Bz*B{0Bz^B|jBcB�B��B�zB�fB�fB��B�fB��B��B��B��B��B��B��B��B��B��B�B��B��B��B�B�B��B�;B��B�4B��B��B��B��B��B��B��B��B��B��B�B��B�	B�DB̘BϫB�YB�eBچB��BݲB��B��B�
B�B�IB�B��B��B��B�2B�B�8B�XB�0B�B�PB�B	;B	'B	'B	GB	3B	MB	aB	 �B�}B	 iB	UB	9B	YB	tB	tB	zB		lB	
rB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	B	)B	 B	$B	'8B	*KB	*�B	/OB	1[B	2|B	4�B	:�B	=�B	>�B	?�B	A�B	B�B	C�B	F�B	G�B	H�B	H�B	I�B	H�B	H�B	HB	I7B	MB	PB	S&B	VB	ZB	^OB	abB	e�B	j�B	o�B	q�B	r-B	rB	r�B	t�B	uB	vB	xB	xB	y	B	z*B	}<B	}<B	�iB	�aB	�mB	��B	�jB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�$B	�*B	�KB	�B	�"B	�=B	�CB	�MB	�TB	�nB	�ZB	�`B	�`B	��B	��B	��B	��B	��B	B	ĜB	ŢB	��B	��B	��B	��B	��B	��B	�B	�B	��B	�B	��B	��B	��B	�B	�B	�B	�B	� B	�B	�B	�B	�FB	�gB	�?B	�B	�=B	�WB	�WB	�WB	�]B	�CB	�]B	�IB	�OB	�OB	ބB	��B	�vB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	��B	��B	�!B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	�	B	��B	��B	�B	�B	�B	�B	�JG�O�B	�qB
 B
;B
 B
 iB
 iB
AB
-B
GB
{B
MB
SB
EB
zB
KB
�B
	lB
	�B
�B
�B
xB
�B
�B
�B
vB
vB
}B
}B
�B
�B
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
B
 B
"�B
$B
$B
#�B
%B
%B
%B
&B
'B
'B
(�B
)�B
*B
*B
)�B
*0B
+B
+B
+B
+B
+6B
+QB
,=B
-)B
-]B
.cB
/5B
/OB
/OB
/OB
/iB
0;B
1AB
1AB
2GB
2GB
33B
3hB
4nB
4TB
4TB
4TB
4nB
4nB
3hB
3hB
3�B
4TB
5tB
6zB
7�B
7fB
7LB
7LB
7fB
7fB
7LB
7fB
7�B
7�B
7�B
7�B
7�B
8�B
9�B
9�B
9�B
9rB
9rB
9rB
9rB
9�B
9�B
:xB
:�B
:xB
9�B
9�B
9�B
;dB
;B
<jB
;dB
;B
:�B
<�B
<�B
<�B
<jB
<�B
<�B
<jB
<�B
<�B
=qB
=�B
=�B
>�B
>�B
?�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
B�B
C�B
C�B
D�B
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
F�B
F�B
G�B
G�B
G�B
H�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
J�B
K�B
K�B
K�B
K�B
M�B
NB
M�B
M�B
M�B
OB
N�B
OB
PB
PB
O�B
PB
QB
Q B
P�B
QB
QB
QB
RB
RB
R B
RB
RB
RB
S&B
RTB
TB
U2B
VB
UB
W$B
V9B
V9B
W$B
W?B
XB
XB
X+B
X+B
XEB
XEB
XEB
YKB
X+B
XEB
X_B
YKB
ZQB
[#B
[=B
[#B
[=B
[WB
[WB
\]B
\CB
\CB
\CB
\CB
\]B
\]B
\CB
]IB
]IB
]IB
]IB
]IB
^jB
^jB
_pB
_pB
`BB
_pB
_VB
_pB
_VB
_VB
`vB
_VB
_pB
`vB
a|B
aHB
abB
abB
`vB
`vB
abB
bhB
b�B
cnB
c�B
dZB
cnB
c�B
dtB
e�B
e�B
ezB
f�B
f�B
f�B
gmB
g�B
g�B
gmB
gmB
g�B
gmB
g�B
g�B
g�B
g�B
g�B
g�B
f�B
iyB
i�B
i�B
i�B
i�B
jB
i�B
j�B
j�B
j�B
j�B
j�B
k�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
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
n�B
o�B
o�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�1111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<0�m<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.11(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201801120033112018011200331120180112003311201806221236012018062212360120180622123601201804050432262018040504322620180405043226  JA  ARFMdecpA19c                                                                20180108093515  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180108003517  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180108003519  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180108003519  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180108003520  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180108003520  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180108003520  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180108003520  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180108003520  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180108003521                      G�O�G�O�G�O�                JA  ARUP                                                                        20180108005525                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180108153349  CV  JULD            G�O�G�O�F��                JM  ARGQJMQC2.0                                                                 20180108153349  CV  JULD_LOCATION   G�O�G�O�F��                JM  ARGQJMQC2.0                                                                 20180108153349  CV  LONGITUDE       G�O�G�O��"�%                JM  ARSQJMQC2.0                                                                 20180109000000  CF  PSAL_ADJUSTED_QCB�  D�� G�O�                JM  ARCAJMQC2.0                                                                 20180111153311  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180111153311  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404193226  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622033601  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115121516                      G�O�G�O�G�O�                