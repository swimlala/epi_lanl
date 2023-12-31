CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T22:56:30Z creation;2022-06-04T22:56:31Z conversion to V3.1      
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
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20220604225630  20220609221504  4902988                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               `A   JA                                  2B  A   APEX                            8600                            2.11.3                          846 @��d���j1   @��d�ʆ@<�M����c��E��1   GPS     A   A   F   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @   @�  @�  A   AffA>ffA`  A�  A�  A�33A���A�  A�33A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BO��BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  C   C  C�C  C�fC
�C  C  C  C  C  C  C�fC  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj�Cl  Cn  Co�fCr  Ct  Cv  Cx  Cy�fC|  C}�fC�  C�  C�  C�  C��C��C�  C�  C��C�  C�  C�  C�  C��C�  C��3C�  C��C��C�  C��3C��3C�  C�  C�  C��3C��3C�  C��C��C�  C�  C�  C��3C��3C��C��C��C�  C��3C��3C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C��C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C��3C��3C��3C��3C�  C��C�  C��3C�  C�  C��3C�  C�  C��C�  C�  C�  C�  C��3C�  C��C�  C��3C�  C��C��C�  C�  C��3C�  C�  C�  C�  C��3C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D	  D	� D
fD
� D  D� D  D�fD  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D��D� D��Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-y�D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D5��D6y�D7  D7�fD8  D8� D9  D9� D:fD:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC�fDDfDD�fDE  DE�fDF  DF� DF��DGy�DG��DHy�DH��DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQfDQ� DRfDR� DS  DS� DT  DT� DT��DUy�DU��DVy�DV��DW� DX  DX� DYfDY�fDZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� D`��Da� Db  Db� Dc  Dc� Dd  Dd� DefDe� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dny�Do  Do� Dp  Dp� Dq  Dq� Dq��Dr� DsfDs� Ds��Dty�Du  Du� Dv  Dv� Dw  Dw� Dx  Dxy�Dy  Dy� Dz  Dz� D{  D{�fD|  D|� D}  D}� D~  D~y�D  D� D�  D�@ D�� D�� D���D�@ D��3D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D��3D�3D�C3D�� D�� D�3D�C3D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D���D���D�@ D�� D�� D�  D�@ D��3D��3D�3D�C3D��3D�� D���D�@ D�� D�� D�  D�@ D�� D��3D�3D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D��3D�� D���D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D��3D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ Dļ�D�  D�C3Dŀ D�� D�  D�@ D�|�D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�<�Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ D�|�D�� D�  D�@ D׀ D׼�D���D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D��3D�  D�<�D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D��3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D�� D�  D�@ D� D�� D���D�@ D� D�� D�  D�@ D� D��3D�  D�@ D� D�� D�  D�@ D� D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�  D�<�D�|�D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�0 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@x��@�z�@�z�A��A<��A^=qA~=qA��A�Q�A��A��A�Q�A��A��A��B�\B�\B�\B�\B'�\B/�\B7�\B?�\BG�\BO(�BW�\B_�\Bg�\Bo�\Bw�\B�\B�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB��{B�ǮB�ǮBÔ{B�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�{B�ǮB�ǮB�ǮB�ǮB�ǮC��C�qC��C�=C	�qC��C��C��C��C��C��C�=C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci�qCk��Cm��Co�=Cq��Cs��Cu��Cw��Cy�=C{��C}�=C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C��C��C���C���C���C��C��C���C���C���C���C���C���C��C��C���C���C���C���C��C��C���C���C��C��C���C���C���C���C���C���C���C��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C��C��C��C��C���C���C���C��C���C���C��C���C���C���C���C���C���C���C��C���C���C���C��C���C���C���C���C���C��C���C���C���C���C��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D x�D ��Dx�D��Dx�D��Dx�D��Dx�D�Dx�D��Dx�D��Dx�D��Dx�D��D	x�D	�\D
x�D
��Dx�D��D\D��Dx�D��Dr�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dr�D�Dx�D�Dr�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��D x�D ��D!x�D!��D"x�D"��D#x�D#��D$x�D$��D%x�D%��D&x�D&��D'x�D'��D(x�D(��D)x�D)��D*x�D*��D+x�D+��D,x�D,��D-r�D-��D.x�D.��D/x�D/��D0x�D0��D1x�D1��D2x�D2��D3x�D3��D4x�D4��D5x�D5�D6r�D6��D7\D7��D8x�D8��D9x�D9�\D:x�D:��D;x�D;��D<x�D<��D=x�D=��D>x�D>��D?x�D?��D@x�D@��DAx�DA��DBx�DB��DC\DC�\DD\DD��DE\DE��DFx�DF�DGr�DG�DHr�DH�DIx�DI��DJx�DJ��DKx�DK��DLx�DL��DMx�DM��DNx�DN��DOx�DO��DPx�DP�\DQx�DQ�\DRx�DR��DSx�DS��DTx�DT�DUr�DU�DVr�DV�DWx�DW��DXx�DX�\DY\DY��DZx�DZ��D[x�D[��D\x�D\��D]x�D]��D^x�D^��D_x�D_��D`x�D`�Dax�Da��Dbx�Db��Dcx�Dc��Ddx�Dd�\Dex�De��Dfx�Df��Dgx�Dg��Dhx�Dh��Dix�Di��Djx�Dj��Dkx�Dk��Dlx�Dl��Dmx�Dm��Dnr�Dn��Dox�Do��Dpx�Dp��Dqx�Dq�Drx�Dr�\Dsx�Ds�Dtr�Dt��Dux�Du��Dvx�Dv��Dwx�Dw��Dxr�Dx��Dyx�Dy��Dzx�Dz��D{\D{��D|x�D|��D}x�D}��D~r�D~��Dx�D��D�<{D�|{D��{D��HD�<{D��D��{D��{D�<{D�|{D���D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��HD��{D�?�D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�yHD��{D��{D�<{D�|{D��HD��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��HD��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�yHD��{D��{D�<{D�|{D���D���D�?�D�|{D��{D���D�?�D�|{D��{D���D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��HD��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�yHD��HD��HD�<{D�|{D��{D��{D�<{D��D���D���D�?�D��D��{D��HD�<{D�|{D��{D��{D�<{D�|{D���D���D�<{D�|{D��HD��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�?�D��D��{D��HD�<{D�|{D��{D��{D�?�D�|{D��{D��{D�<{D��D��{D��{D�9HD�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D���D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�?�D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��HD��{D�<{D�|{D��{D��{D�9HD�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D¼{D��{D�<{D�|{Dü{D��{D�<{D�|{DĹHD��{D�?�D�|{Dż{D��{D�<{D�yHDƼ{D��{D�<{D�|{DǼ{D��{D�<{D�|{Dȼ{D��{D�<{D�|{Dɼ{D��{D�9HD�|{Dʼ{D��{D�<{D�|{D˼{D��{D�<{D�|{D̼{D��{D�<{D�|{Dͼ{D��{D�<{D�|{Dμ{D��{D�<{D�|{Dϼ{D��{D�<{D�|{Dм{D��{D�<{D�|{DѼ{D��{D�<{D�|{DҼ{D��{D�<{D�|{DӼ{D��{D�<{D�|{DԼ{D��{D�<{D�|{Dռ{D��{D�<{D�yHDּ{D��{D�<{D�|{D׹HD��HD�<{D�|{Dؼ{D��{D�<{D�|{Dټ{D��{D�<{D�|{Dڼ{D��{D�<{D�|{Dۼ{D��{D�<{D�|{Dܿ�D��{D�9HD�|{Dݼ{D��{D�<{D�|{D޼{D��{D�<{D�|{D߼{D��{D�<{D��D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�yHD�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D��D�{D��{D�<{D�|{D�{D��HD�<{D�|{D�{D��{D�<{D�|{D뿮D��{D�<{D�|{D�{D��{D�<{D�|{D���D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D���D��{D�9HD�yHD��{D��{D�<{D��D��{D��{D�<{D�|{D��{D��{D�9HD�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�,{111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�ȴA��?A�{A�Y�A�Q�A�y	A��>A���A���A�VA��A���A�v�A�ffA�[�A�U�A�L0A�>A�&�A���A�{A�33A�C�A���A�3hA�A�{�A��A���A�5A��
A��kA�($A���A�bA�bA��XA�O�A��A�r�A���A�$A�I�A�уA���A���A� 'A�r�A�$�A���A�VA��A���A�<jA�S[A�VA�`vA�.A�r�A�-A��>A�� A�9�A�_�A���A��[A���A���A�{JA��A��xA�aHA��sA�w�A�x�A��A�DA���A���A�49A�,�A���A�BA0�A~�YA}�=A{��Az6Ax��Awt�Av��Au�Asy>AqJ�An�qAmS&Akj�Ak�Aj��Ai�Ai[WAh��AhXAg�PAf�Ad%A`�NA^�HA]�[A]HA\��A[��A[+AZ�KAY��AX��AV�.AT��ATv`ATa|AT:*ATASԕAS*�AQ�QAP�BAO�pAO�{AOXyAO1�AN�rAM��AL�0ALv`AK�@AKl"AK_AJ�8AJ/�AI��AHz�AG�AG��AGc�AG+AG�AFbNAE��AEADa�AC�$AC&AB��AB��AA��AAffA@ȴA@�A@qvA?�	A?p�A>��A>.�A=�)A=ƨA<�A;1�A: �A9;dA8�A8�tA8xA8VmA8�A7��A62�A5S&A4Q�A3� A2��A1��A0�nA/��A.�yA-�A+��A)�A(:�A'��A&��A$�A#�MA"S�A"&�A!c�A ��A J�A��A:*A��A��A�kA��A��A��A��A��A�wA�YAp�ARTAN<AMjAS�AMA@A�VA��A��A��A�}A��A�A33Av�Ag�A�8A��A�A*�A
�.A
0UA	��A	�A�fA�AS&A�TA��AxA 1�@�x@��]@��c@��U@�e@��@��o@�g�@���@��	@�=q@�n@��@��@�	l@�|�@�s�@�@�>�@� �@�P@�p�@�%F@��E@��@滙@���@�O@�6@��@�o @��@�@�N<@�	@� i@��T@�d�@�$@�ƨ@�K^@�G�@���@ѨX@ѹ�@ў�@�'�@�\�@ω7@�Ft@�zx@̗�@̖�@̈�@ˉ7@ʲ�@ɾw@�c@�p�@��@Ȑ.@ơb@��"@�L0@�#:@��+@��@���@��@��@��@���@�@���@��L@�g8@�]d@�U2@�Ov@�GE@��@�v`@��x@�	@��[@�dZ@�$t@�=@��P@���@�g8@�M@�:�@�J@���@���@��@�Q�@���@�S&@���@�?@��n@���@��a@��@��@�+@��@���@�:*@���@��@��[@��h@���@�9�@���@�J�@��}@���@�X@��@��[@���@�^�@��@�ߤ@�w�@��$@��@��t@�͟@�u�@�G@��Q@��X@���@��)@��"@�F@���@��}@�� @�=q@��N@���@���@�~(@��@��@�n/@�7L@�@��5@�p;@���@��C@��@��@�C�@��;@���@�Mj@��@���@�%�@�J@���@��H@��M@�+@�Y@��@��@�Ĝ@�rG@�&@�E9@��@���@�6@�g8@�u@��@�!�@�.I@��@�*�@��j@�s@�@�Q@��@��@���@�P�@�@�y>@�,=@���@�m]@�;@��z@� �@~�F@~O@}��@}�~@}O�@}�S@}�d@|��@{�]@|$@|x@{(@z��@z{�@z}V@z~�@z��@z�A@z��@z��@z��@y�T@yV@x�E@x�u@x�@ws@wW?@w1�@w(@v��@v�y@v�@v�c@v�'@vL0@u:�@u�@t��@s�@s�4@sqv@s]�@s,�@r�,@r��@r��@rE�@r&�@rO@r@q�.@q�h@qJ�@p�@p�@pD�@p1'@px@p�@o�r@o�@o��@n��@n��@nH�@m�-@m@l��@l�O@l�e@l��@l�o@lbN@l1@k��@k�@@k��@k9�@j�y@j�@j��@j�6@j��@jR�@i��@iN<@hg8@h2�@g��@g�@g�@g�Q@gg�@go@gS@f^5@e��@e�@e�@e��@e��@d��@d��@d�.@d]d@d(�@c�@c�6@c|�@c"�@b� @bkQ@bO@a�@a�H@a��@ao @a7L@`�	@`Ɇ@`l"@_�
@_�	@_Mj@_(@^��@^W�@]��@]��@]@\��@\�Y@\y>@\l"@\D�@[خ@[]�@[;d@[�@ZR�@Y�@Y��@Y�@X�I@X��@X�Y@XU2@XD�@X �@W��@W�k@WZ�@V��@U�@U��@U��@U}�@U[W@T�P@T�o@Toi@T:�@S��@SO@R��@RZ�@R+k@R{@Q��@P�[@P��@Poi@P-�@P�@P@O��@O�4@O\)@O�@N�s@N��@M��@M�@Ms�@MJ�@M+@L�U@LbN@L �@L�@K�A@K�0@J�@Jxl@J?@J-@Ju@I�D@I�#@I��@I�"@IX@H��@H�z@H,=@G�a@G�@G�@@G��@G��@G�*@G��@G��@GC�@GS@F�2@F�F@F+k@E�)@E��@EY�@E0�@E�@D�/@D��@DPH@C��@C�@C�{@B��@B\�@B)�@B�@A�@A�@A�3@Ac@@�$@?��@?�6@?�}@?خ@?�F@?��@?|�@?.I@>.�@=��@=m]@<��@<|�@<7@;��@;4�@:�@:_�@:{@9�Z@9�T@9�j@9�@9@9��@9w2@9%F@9�@8��@8�5@8�v@8��@8��@8�@7b�@7�@6�L@6GE@5�X@5p�@5o @54@4�[@4�_@4U2@4>B@4-�@3�r@3x@3W?@333@2��@2@�@2
�@1�>@1�>@1��@1�j@1�@1�d@1�@1��@1x�@1<6@0�j@/�K@/{J@/\)@/S�@/&@.�8@.��@.��@.�@-��@-f�@,��@,��@,q@,bN@,PH@,A�@+x@+o@+�@*��@*n�@*V@*=q@)��@)��@)T�@)B�@)/@(�@(�Y@(]d@(1'@'��@'33@'C@&��@&ߤ@&�m@&��@&��@&-@&4@&u@%�@%�#@%@%��@%��@%B�@%�@$��@$�j@$I�@#�a@#C@"�@"H�@!��@!�@"_@!}�@ ѷ@ ��@ q@ PH@ 4n@�]@�:@W?@$t@��@z@Z�@=q@($@&�@�@�o@��@�@T�@Dg@�P@�4@e�@H@*�@�@�@"h@�@�@�@�@7@�&@\)@Mj@'�@��@��@{�@3�@0U@�@	@ϫ@��@zx@[W@5�@+@�	@��@`�@9X@�r@�6@A�@@��@�H@�,@�!@v�@1�@_@��@��@�@��@L�@=�@q@�p@I�@$@�@G@�}@��@&@�H@�@�'@�h@��@��@��@� @Ov@u@��@Vm@4@%F@�@��@q@e�@bN@V�@:�@�6@�f@l�@\)@�@�@ȴ@��@�1@��@p;@u%@W�@C�@8�@!�@@4@J@��@�@��@}�@m]@e,@^�@Vm@@֡@Ĝ@��@��@M@-�@	�@�}@'�@
��@
��@
��@
u%@
3�@
e@	��@	�@	�@	�t@	��@	m]@	Y�@	G�@	5�@	q@�P@�f@�@�j@c�@V�@D�@-�@-�@�@�+@��@e�@F�@6z@+@&@�@Y@�@ i@��@ȴ@�F@8�@�j@�=@��@�M@e,@*0@�@�@g8@,=@x@�w@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�ȴA��?A�{A�Y�A�Q�A�y	A��>A���A���A�VA��A���A�v�A�ffA�[�A�U�A�L0A�>A�&�A���A�{A�33A�C�A���A�3hA�A�{�A��A���A�5A��
A��kA�($A���A�bA�bA��XA�O�A��A�r�A���A�$A�I�A�уA���A���A� 'A�r�A�$�A���A�VA��A���A�<jA�S[A�VA�`vA�.A�r�A�-A��>A�� A�9�A�_�A���A��[A���A���A�{JA��A��xA�aHA��sA�w�A�x�A��A�DA���A���A�49A�,�A���A�BA0�A~�YA}�=A{��Az6Ax��Awt�Av��Au�Asy>AqJ�An�qAmS&Akj�Ak�Aj��Ai�Ai[WAh��AhXAg�PAf�Ad%A`�NA^�HA]�[A]HA\��A[��A[+AZ�KAY��AX��AV�.AT��ATv`ATa|AT:*ATASԕAS*�AQ�QAP�BAO�pAO�{AOXyAO1�AN�rAM��AL�0ALv`AK�@AKl"AK_AJ�8AJ/�AI��AHz�AG�AG��AGc�AG+AG�AFbNAE��AEADa�AC�$AC&AB��AB��AA��AAffA@ȴA@�A@qvA?�	A?p�A>��A>.�A=�)A=ƨA<�A;1�A: �A9;dA8�A8�tA8xA8VmA8�A7��A62�A5S&A4Q�A3� A2��A1��A0�nA/��A.�yA-�A+��A)�A(:�A'��A&��A$�A#�MA"S�A"&�A!c�A ��A J�A��A:*A��A��A�kA��A��A��A��A��A�wA�YAp�ARTAN<AMjAS�AMA@A�VA��A��A��A�}A��A�A33Av�Ag�A�8A��A�A*�A
�.A
0UA	��A	�A�fA�AS&A�TA��AxA 1�@�x@��]@��c@��U@�e@��@��o@�g�@���@��	@�=q@�n@��@��@�	l@�|�@�s�@�@�>�@� �@�P@�p�@�%F@��E@��@滙@���@�O@�6@��@�o @��@�@�N<@�	@� i@��T@�d�@�$@�ƨ@�K^@�G�@���@ѨX@ѹ�@ў�@�'�@�\�@ω7@�Ft@�zx@̗�@̖�@̈�@ˉ7@ʲ�@ɾw@�c@�p�@��@Ȑ.@ơb@��"@�L0@�#:@��+@��@���@��@��@��@���@�@���@��L@�g8@�]d@�U2@�Ov@�GE@��@�v`@��x@�	@��[@�dZ@�$t@�=@��P@���@�g8@�M@�:�@�J@���@���@��@�Q�@���@�S&@���@�?@��n@���@��a@��@��@�+@��@���@�:*@���@��@��[@��h@���@�9�@���@�J�@��}@���@�X@��@��[@���@�^�@��@�ߤ@�w�@��$@��@��t@�͟@�u�@�G@��Q@��X@���@��)@��"@�F@���@��}@�� @�=q@��N@���@���@�~(@��@��@�n/@�7L@�@��5@�p;@���@��C@��@��@�C�@��;@���@�Mj@��@���@�%�@�J@���@��H@��M@�+@�Y@��@��@�Ĝ@�rG@�&@�E9@��@���@�6@�g8@�u@��@�!�@�.I@��@�*�@��j@�s@�@�Q@��@��@���@�P�@�@�y>@�,=@���@�m]@�;@��z@� �@~�F@~O@}��@}�~@}O�@}�S@}�d@|��@{�]@|$@|x@{(@z��@z{�@z}V@z~�@z��@z�A@z��@z��@z��@y�T@yV@x�E@x�u@x�@ws@wW?@w1�@w(@v��@v�y@v�@v�c@v�'@vL0@u:�@u�@t��@s�@s�4@sqv@s]�@s,�@r�,@r��@r��@rE�@r&�@rO@r@q�.@q�h@qJ�@p�@p�@pD�@p1'@px@p�@o�r@o�@o��@n��@n��@nH�@m�-@m@l��@l�O@l�e@l��@l�o@lbN@l1@k��@k�@@k��@k9�@j�y@j�@j��@j�6@j��@jR�@i��@iN<@hg8@h2�@g��@g�@g�@g�Q@gg�@go@gS@f^5@e��@e�@e�@e��@e��@d��@d��@d�.@d]d@d(�@c�@c�6@c|�@c"�@b� @bkQ@bO@a�@a�H@a��@ao @a7L@`�	@`Ɇ@`l"@_�
@_�	@_Mj@_(@^��@^W�@]��@]��@]@\��@\�Y@\y>@\l"@\D�@[خ@[]�@[;d@[�@ZR�@Y�@Y��@Y�@X�I@X��@X�Y@XU2@XD�@X �@W��@W�k@WZ�@V��@U�@U��@U��@U}�@U[W@T�P@T�o@Toi@T:�@S��@SO@R��@RZ�@R+k@R{@Q��@P�[@P��@Poi@P-�@P�@P@O��@O�4@O\)@O�@N�s@N��@M��@M�@Ms�@MJ�@M+@L�U@LbN@L �@L�@K�A@K�0@J�@Jxl@J?@J-@Ju@I�D@I�#@I��@I�"@IX@H��@H�z@H,=@G�a@G�@G�@@G��@G��@G�*@G��@G��@GC�@GS@F�2@F�F@F+k@E�)@E��@EY�@E0�@E�@D�/@D��@DPH@C��@C�@C�{@B��@B\�@B)�@B�@A�@A�@A�3@Ac@@�$@?��@?�6@?�}@?خ@?�F@?��@?|�@?.I@>.�@=��@=m]@<��@<|�@<7@;��@;4�@:�@:_�@:{@9�Z@9�T@9�j@9�@9@9��@9w2@9%F@9�@8��@8�5@8�v@8��@8��@8�@7b�@7�@6�L@6GE@5�X@5p�@5o @54@4�[@4�_@4U2@4>B@4-�@3�r@3x@3W?@333@2��@2@�@2
�@1�>@1�>@1��@1�j@1�@1�d@1�@1��@1x�@1<6@0�j@/�K@/{J@/\)@/S�@/&@.�8@.��@.��@.�@-��@-f�@,��@,��@,q@,bN@,PH@,A�@+x@+o@+�@*��@*n�@*V@*=q@)��@)��@)T�@)B�@)/@(�@(�Y@(]d@(1'@'��@'33@'C@&��@&ߤ@&�m@&��@&��@&-@&4@&u@%�@%�#@%@%��@%��@%B�@%�@$��@$�j@$I�@#�a@#C@"�@"H�@!��@!�@"_@!}�@ ѷ@ ��@ q@ PH@ 4n@�]@�:@W?@$t@��@z@Z�@=q@($@&�@�@�o@��@�@T�@Dg@�P@�4@e�@H@*�@�@�@"h@�@�@�@�@7@�&@\)@Mj@'�@��@��@{�@3�@0U@�@	@ϫ@��@zx@[W@5�@+@�	@��@`�@9X@�r@�6@A�@@��@�H@�,@�!@v�@1�@_@��@��@�@��@L�@=�@q@�p@I�@$@�@G@�}@��@&@�H@�@�'@�h@��@��@��@� @Ov@u@��@Vm@4@%F@�@��@q@e�@bN@V�@:�@�6@�f@l�@\)@�@�@ȴ@��@�1@��@p;@u%@W�@C�@8�@!�@@4@J@��@�@��@}�@m]@e,@^�@Vm@@֡@Ĝ@��@��@M@-�@	�@�}@'�@
��@
��@
��@
u%@
3�@
e@	��@	�@	�@	�t@	��@	m]@	Y�@	G�@	5�@	q@�P@�f@�@�j@c�@V�@D�@-�@-�@�@�+@��@e�@F�@6z@+@&@�@Y@�@ i@��@ȴ@�F@8�@�j@�=@��@�M@e,@*0@�@�@g8@,=@x@�w@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�B�BB�BB��B�BDB��B�B�BjB�B[B@B�B�BFBgB�BB�BB��B�B�B��B�BܒB�HB�B�}B�DB�B��B�BB��B��B�RB�mB��B��B��Br-B\�BT�BTFBG_B=qB6zB1�B.�B#�BB��B�rB��B�B��B�.B�xB��B�2B�	B�B��B��B�lBv`Bg�B^�BVmBCGB7LB.IB$tB�B�BbBB3B�B
��B
��B
�iB
�eB
��B
�8B
�&B
�7B
��B
�4B
��B
ƨB
��B
�zB
��B
�B
�2B
�hB
�pB
��B
�B
��B
��B
~�B
y�B
p�B
dZB
X�B
TaB
PbB
M�B
JXB
D�B
B�B
<�B
4�B
1vB
$�B
# B
#B
!HB
 vB
�B
�B
�B
�B
4B
�B
�B
�B
B
	lB
�B	�B	��B	�B	�B	�B	�B	��B	�B	�LB	�B	��B	��B	��B	�B	�)B	�WB	�sB	��B	ҽB	уB	��B	�~B	�	B	��B	��B	�B	�B	��B	��B	�cB	�cB	��B	�B	��B	��B	�B	�KB	��B	�
B	��B	�B	�NB	��B	��B	��B	�DB	�B	�3B	~�B	z�B	v�B	r|B	k�B	c B	]~B	[=B	W?B	PB	J�B	CaB	BuB	?.B	<�B	:DB	9	B	6`B	4�B	2|B	-B	*�B	(XB	'B	 \B	�B	�B	YB	�B	�B	�B	�B	mB	9B	�B	�B	�B	 B	6B	dB	)B	
XB	YB	�B	�B�B�B��B��B��B��B�B�IB��B�$B�B��BݲB�B�B�B��B��B׍B�mB�B՛B�2B�BөB�B�bBϫBΥB͟B��B�B˒BʌB�B��B�B�BȚB�KBǔB�zBǮB�?BżB�9B�9B�B��BðB��B��B��B��B�B��B��B��B��B��B��B�'BªB�gB��B��B��BĜB��B�mBĶBÖB�SBňBŢB�3B��BňB�BĜB�gBňBāB�?B�B�_B�+B�fB�lBʦB�XBʌBʦB��BʦBˬB�6BңB�,B�{B�aB�B��BרB��BٴBٚB�B��B��B�CB�/B޸B�!B��B�HB�B�B�&B�zB�kB�B�B�B��B��B�B�B��B��B	aB	MB	%B	�B	zB		�B	DB	�B	�B	�B	�B	VB	B	 B	4B	�B	kB	B	�B	 vB	 �B	!B	$�B	'mB	(�B	)DB	*B	*�B	*�B	+�B	-)B	1�B	33B	33B	6`B	8�B	:�B	;�B	<PB	=B	@ B	C�B	DgB	H�B	JrB	J�B	L~B	OB	R�B	R�B	UMB	W�B	X�B	Y1B	Z�B	\�B	`B	`�B	`�B	`\B	b�B	kB	r-B	y$B	~(B	�'B	��B	��B	�~B	��B	�aB	��B	��B	�hB	��B	��B	��B	�zB	�B	�^B	��B	�OB	�'B	ǮB	�	B	�B	өB	յB	�mB	��B	ٚB	��B	�xB	��B	��B	�B	�B	�nB	�B	��B	�wB	��B	�B	�TB	�TB	�nB	�nB	�nB	�B	�B	�nB	�RB	��B	�VB	��B	��B	�}B
�B
�B
�B
GB
9B
EB
EB
�B
	lB
�B
PB
B
B
.B
bB
�B
4B
oB
{B
�B
$B
�B
�B
�B
EB
B
QB
�B
B
 �B
#B
$�B
%�B
&�B
)*B
*KB
+B
+kB
-�B
0;B
2-B
2�B
3�B
49B
4�B
4�B
5B
6B
7�B
7�B
8�B
:DB
;�B
<�B
=<B
=�B
=�B
?�B
AUB
C�B
E�B
F�B
HB
I�B
KDB
NpB
O�B
RoB
S�B
V�B
X�B
ZB
Z�B
[#B
[qB
^B
^OB
_VB
`BB
a-B
a�B
b�B
dB
ezB
g�B
h$B
iyB
kQB
l�B
m�B
ncB
o�B
poB
q'B
r�B
u%B
v`B
wfB
xlB
y�B
{0B
|�B
~(B
�OB
��B
�[B
��B
��B
�GB
��B
�%B
�B
�YB
�fB
��B
�rB
��B
��B
��B
�B
�"B
�VB
�<B
��B
� B
�oB
��B
��B
��B
�EB
�_B
�yB
��B
�	B
�qB
�)B
��B
��B
�BB
�HB
��B
��B
�B
�zB
�B
��B
�B
�B
�8B
�mB
��B
��B
�>B
�sB
��B
��B
��B
��B
�CB
��B
� B
�!B
��B
��B
�AB
��B
�B
��B
�tB
��B
��B
��B
�FB
�`B
��B
�LB
�B
��B
�*B
�B
�JB
�B
��B
�B
�B
�B
��B
��B
�B
�VB
�(B
�.B
��B
�OB
��B
�UB
��B
�B
ªB
�-B
�3B
��B
�B
�?B
��B
�B
ȀB
ȴB
ȚB
��B
�RB
�)B
��B
��B
��B
̳B
�B
�B
�jB
�"B
��B
�.B
� B
��B
�:B
��B
�aB
�2B
��B
�
B
רB
רB
��B
��B
��B
�+B
�EB
�_B
��B
�B
�B
�B
�7B
چB
چB
��B
�B
�~B
�B
ޞB
߾B
��B
߾B
�'B
��B
�HB
��B
��B
�B
�B
�B
��B
�B
��B
�`B
��B
��B
��B
��B
�B
�B
�LB
�B
�LB
�B
��B
��B
�_B
��B
��B
��B
�eB
�B
�B
�B
�6B
�B
�"B
��B
�B
�)B
�)B
�CB
�CB
�IB
��B
�iB
�B
�AB
�B
�B
�B
��B
�B
�3B
�3B
��B
�B
�B
��B
��B
�FB
�zB
��B
��B
�B
�LB
�fB
�8B
�B
�8B
�8B
�lB
��B
��B
��B
�XB
��B
��B
�xB
�0B
��B
��B
�"B
��B
�BB
�BB
��B
��B
��B
��B  B B 4B �BBoB�B[B�B�B�BB�BBGB{B�B�B�B�B�BB9BmB�B�B�B�B�B�B�BmB�B�B�B�BB_B�BKB1BKBfB�B�B	B	B	lB	lB	�B
#B
XB
�B
�BB0B�B�B�BBBjB�B�B<BVBVB�B�B�BBBB�B�B�B�BB4B�BTB�B�B�B�B�B�B�BB�B,BaB�B�B�BgB�BBBBSB$B�B�B�ByB�B1BeB�B�BBQB�B�B#BWBWB=BWBqB�B�BCBCBCBCBCB�B�BBBdB�B�B�BB�B;B;B�B�B 'B vB �B �B �B �B!-B!bB!|B!�B!�B!�B"B"4B"B"NB# B# B#TB#nB#TB#�B#�B$�B$�B$�B$�B$�B$�B$�B$�B$�B$�B$�B%,B%`B%�B&2B&fB&fB&fB&�B'B'8B'�B'�B'�B(
B(�B)444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444   B�B�BB�BB��B�BDB��B�B�BjB�B[B@B�B�BFBgB�BB�BB��B�B�B��B�BܒB�HB�B�}B�DB�B��B�BB��B��B�RB�mB��B��B��Br-B\�BT�BTFBG_B=qB6zB1�B.�B#�BB��B�rB��B�B��B�.B�xB��B�2B�	B�B��B��B�lBv`Bg�B^�BVmBCGB7LB.IB$tB�B�BbBB3B�B
��B
��B
�iB
�eB
��B
�8B
�&B
�7B
��B
�4B
��B
ƨB
��B
�zB
��B
�B
�2B
�hB
�pB
��B
�B
��B
��B
~�B
y�B
p�B
dZB
X�B
TaB
PbB
M�B
JXB
D�B
B�B
<�B
4�B
1vB
$�B
# B
#B
!HB
 vB
�B
�B
�B
�B
4B
�B
�B
�B
B
	lB
�B	�B	��B	�B	�B	�B	�B	��B	�B	�LB	�B	��B	��B	��B	�B	�)B	�WB	�sB	��B	ҽB	уB	��B	�~B	�	B	��B	��B	�B	�B	��B	��B	�cB	�cB	��B	�B	��B	��B	�B	�KB	��B	�
B	��B	�B	�NB	��B	��B	��B	�DB	�B	�3B	~�B	z�B	v�B	r|B	k�B	c B	]~B	[=B	W?B	PB	J�B	CaB	BuB	?.B	<�B	:DB	9	B	6`B	4�B	2|B	-B	*�B	(XB	'B	 \B	�B	�B	YB	�B	�B	�B	�B	mB	9B	�B	�B	�B	 B	6B	dB	)B	
XB	YB	�B	�B�B�B��B��B��B��B�B�IB��B�$B�B��BݲB�B�B�B��B��B׍B�mB�B՛B�2B�BөB�B�bBϫBΥB͟B��B�B˒BʌB�B��B�B�BȚB�KBǔB�zBǮB�?BżB�9B�9B�B��BðB��B��B��B��B�B��B��B��B��B��B��B�'BªB�gB��B��B��BĜB��B�mBĶBÖB�SBňBŢB�3B��BňB�BĜB�gBňBāB�?B�B�_B�+B�fB�lBʦB�XBʌBʦB��BʦBˬB�6BңB�,B�{B�aB�B��BרB��BٴBٚB�B��B��B�CB�/B޸B�!B��B�HB�B�B�&B�zB�kB�B�B�B��B��B�B�B��B��B	aB	MB	%B	�B	zB		�B	DB	�B	�B	�B	�B	VB	B	 B	4B	�B	kB	B	�B	 vB	 �B	!B	$�B	'mB	(�B	)DB	*B	*�B	*�B	+�B	-)B	1�B	33B	33B	6`B	8�B	:�B	;�B	<PB	=B	@ B	C�B	DgB	H�B	JrB	J�B	L~B	OB	R�B	R�B	UMB	W�B	X�B	Y1B	Z�B	\�B	`B	`�B	`�B	`\B	b�B	kB	r-B	y$B	~(B	�'B	��B	��B	�~B	��B	�aB	��B	��B	�hB	��B	��B	��B	�zB	�B	�^B	��B	�OB	�'B	ǮB	�	B	�B	өB	յB	�mB	��B	ٚB	��B	�xB	��B	��B	�B	�B	�nB	�B	��B	�wB	��B	�B	�TB	�TB	�nB	�nB	�nB	�B	�B	�nB	�RB	��B	�VB	��B	��B	�}B
�B
�B
�B
GB
9B
EB
EB
�B
	lB
�B
PB
B
B
.B
bB
�B
4B
oB
{B
�B
$B
�B
�B
�B
EB
B
QB
�B
B
 �B
#B
$�B
%�B
&�B
)*B
*KB
+B
+kB
-�B
0;B
2-B
2�B
3�B
49B
4�B
4�B
5B
6B
7�B
7�B
8�B
:DB
;�B
<�B
=<B
=�B
=�B
?�B
AUB
C�B
E�B
F�B
HB
I�B
KDB
NpB
O�B
RoB
S�B
V�B
X�B
ZB
Z�B
[#B
[qB
^B
^OB
_VB
`BB
a-B
a�B
b�B
dB
ezB
g�B
h$B
iyB
kQB
l�B
m�B
ncB
o�B
poB
q'B
r�B
u%B
v`B
wfB
xlB
y�B
{0B
|�B
~(B
�OB
��B
�[B
��B
��B
�GB
��B
�%B
�B
�YB
�fB
��B
�rB
��B
��B
��B
�B
�"B
�VB
�<B
��B
� B
�oB
��B
��B
��B
�EB
�_B
�yB
��B
�	B
�qB
�)B
��B
��B
�BB
�HB
��B
��B
�B
�zB
�B
��B
�B
�B
�8B
�mB
��B
��B
�>B
�sB
��B
��B
��B
��B
�CB
��B
� B
�!B
��B
��B
�AB
��B
�B
��B
�tB
��B
��B
��B
�FB
�`B
��B
�LB
�B
��B
�*B
�B
�JB
�B
��B
�B
�B
�B
��B
��B
�B
�VB
�(B
�.B
��B
�OB
��B
�UB
��B
�B
ªB
�-B
�3B
��B
�B
�?B
��B
�B
ȀB
ȴB
ȚB
��B
�RB
�)B
��B
��B
��B
̳B
�B
�B
�jB
�"B
��B
�.B
� B
��B
�:B
��B
�aB
�2B
��B
�
B
רB
רB
��B
��B
��B
�+B
�EB
�_B
��B
�B
�B
�B
�7B
چB
چB
��B
�B
�~B
�B
ޞB
߾B
��B
߾B
�'B
��B
�HB
��B
��B
�B
�B
�B
��B
�B
��B
�`B
��B
��B
��B
��B
�B
�B
�LB
�B
�LB
�B
��B
��B
�_B
��B
��B
��B
�eB
�B
�B
�B
�6B
�B
�"B
��B
�B
�)B
�)B
�CB
�CB
�IB
��B
�iB
�B
�AB
�B
�B
�B
��B
�B
�3B
�3B
��B
�B
�B
��B
��B
�FB
�zB
��B
��B
�B
�LB
�fB
�8B
�B
�8B
�8B
�lB
��B
��B
��B
�XB
��B
��B
�xB
�0B
��B
��B
�"B
��B
�BB
�BB
��B
��B
��B
��B  B B 4B �BBoB�B[B�B�B�BB�BBGB{B�B�B�B�B�BB9BmB�B�B�B�B�B�B�BmB�B�B�B�BB_B�BKB1BKBfB�B�B	B	B	lB	lB	�B
#B
XB
�B
�BB0B�B�B�BBBjB�B�B<BVBVB�B�B�BBBB�B�B�B�BB4B�BTB�B�B�B�B�B�B�BB�B,BaB�B�B�BgB�BBBBSB$B�B�B�ByB�B1BeB�B�BBQB�B�B#BWBWB=BWBqB�B�BCBCBCBCBCB�B�BBBdB�B�B�BB�B;B;B�B�B 'B vB �B �B �B �B!-B!bB!|B!�B!�B!�B"B"4B"B"NB# B# B#TB#nB#TB#�B#�B$�B$�B$�B$�B$�B$�B$�B$�B$�B$�B$�B%,B%`B%�B&2B&fB&fB&fB&�B'B'8B'�B'�B'�B(
B(�B)444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604213056  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604225630  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604225631  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604225631                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605075639  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605075639  QCF$                G�O�G�O�G�O�            8000JA  ARUP                                                                        20220609221504                      G�O�G�O�G�O�                