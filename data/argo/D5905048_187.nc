CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2017-12-06T00:35:27Z creation;2017-12-06T00:35:31Z conversion to V3.1;2019-12-19T07:50:57Z update;     
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
_FillValue                    �DArgo profile    3.1 1.2 19500101000000  20171206003527  20200116221515  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0577_187                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @�:�� 1   @�:�>�� @4$�J��d���(1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  @���AffA@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�33B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ�CL  CN  CP  CR  CT  CV  CX  CZ  C\  C]�fC`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D�fDfD� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D
��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� DpfDp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�P 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��H@�z�@�G�A��A>=qA^=qA~=qA��A��A��A��A��A��A��A��B�\B�\B�\B�\B'�\B/�\B7�\B?�\BG�\BO�\BW�\B_�\Bg�\Bo�\Bw�\B�\B�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB��{B�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB���B�ǮB�ǮB���B�ǮB�ǮB�ǮC��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI�qCK��CM��CO��CQ��CS��CU��CW��CY��C[��C]�=C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C��C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D x�D ��D\D�\Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��D	x�D	��D
x�D
�Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��D x�D ��D!x�D!��D"x�D"��D#x�D#��D$x�D$��D%x�D%��D&x�D&��D'x�D'��D(x�D(��D)x�D)��D*x�D*��D+x�D+��D,x�D,��D-x�D-��D.x�D.��D/x�D/��D0x�D0��D1x�D1��D2x�D2��D3x�D3��D4x�D4��D5x�D5��D6x�D6��D7x�D7��D8x�D8��D9x�D9��D:x�D:��D;x�D;��D<x�D<��D=x�D=��D>x�D>��D?x�D?��D@x�D@��DAx�DA��DBx�DB��DCx�DC��DDx�DD��DEx�DE��DFx�DF��DGx�DG��DHx�DH��DIx�DI��DJx�DJ��DKx�DK��DLx�DL��DMx�DM��DNx�DN��DOx�DO��DPx�DP��DQx�DQ��DRx�DR��DSx�DS��DTx�DT��DUx�DU��DVx�DV��DWx�DW��DXx�DX��DYx�DY��DZx�DZ��D[x�D[��D\x�D\��D]x�D]��D^x�D^��D_x�D_��D`x�D`��Dax�Da��Dbx�Db��Dcx�Dc��Ddx�Dd��Dex�De��Dfx�Df��Dgx�Dg��Dhx�Dh��Dix�Di��Djx�Dj��Dkx�Dk��Dlx�Dl��Dmx�Dm��Dnx�Dn��Dox�Do�\Dpx�Dp��Dqx�Dq��Drx�Dr��Dsx�Ds��Dtx�Dt��Dux�Du��Dvx�Dv��Dwx�Dw��Dxx�Dx��Dyx�Dy��Dzx�Dz��D{x�D{��D|x�D|��D}x�D}��D~x�D~��Dx�D��D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��HD��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D¼{D��{D�<{D�|{Dü{D��{D�<{D�|{Dļ{D��{D�<{D�|{Dż{D��{D�<{D�|{DƼ{D��{D�<{D�|{DǼ{D��{D�<{D�|{Dȼ{D��{D�<{D�|{Dɼ{D��{D�<{D�|{Dʼ{D��{D�<{D�|{D˼{D��{D�<{D�|{D̼{D��{D�<{D�|{Dͼ{D��{D�<{D�|{Dμ{D��{D�<{D�|{Dϼ{D��{D�<{D�|{Dм{D��{D�<{D�|{DѼ{D��{D�<{D�|{DҼ{D��{D�<{D�|{DӼ{D��{D�<{D�|{DԼ{D��{D�<{D�|{Dռ{D��{D�<{D�|{Dּ{D��{D�<{D�|{D׼{D��{D�<{D�|{Dؼ{D��{D�<{D�|{Dټ{D��{D�<{D�|{Dڼ{D��{D�<{D�|{Dۼ{D��{D�<{D�|{Dܼ{D��{D�<{D�|{Dݼ{D��{D�<{D�|{D޼{D��{D�<{D�|{D߼{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D���D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�9HD�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D��{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�L{1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A��A��/A��/A��/A��/A��;A��HA��HA��HA��TA��`A��`A��mA��mA��mA��`A��`A��mA��yA��yA��yA��yA��yA��A��yA��yA��yA��`A��#A���A���A�ȴA̸RA̗�A�x�A�C�A��A���A˼jA�1Aʗ�A�K�A���A�5?A��A�A���A�(�A��A�
=A��#A���A�ȴA���A��A�1A���A�9XA�oA�ƨA��A�ĜA�r�A�Q�A���A�VA��9A��^A�G�A��A��\A�ȴA�S�A���A�v�A�5?A�x�A�S�A�O�A��A���A�A��A��/A�bNA�bNA�I�A�|�A�z�A�A�A�A��A��A�XA��A��9A��A�ĜA�XA�  A��A��A�JA�;dA��uA�M�A���A�I�A�`BA��A�l�A��HA�1'A�33A�bA}�A|�`A|=qAy?}Ax1Av1'Ast�Ap��Am�^Ak�^Ait�Ag�AdjA_�wA^  A[�AZZAX�+AV�ATȴAR(�AQ�PAP��APQ�AN�\AMx�AK�hAI/AHffAFz�AE��AE�AD�`AD�DAD^5ADE�ADbABz�A>(�A<�A<I�A;l�A9`BA7%A6r�A6JA5��A5XA4�uA3��A3%A1K�A/�A-��A+�hA*�A*A�A)�-A(bNA'%A&ffA$VA"n�A!?}A ĜA��AVA  A�AA�+A�-A`BA�A��A�RA(�A�AhsA�A��A��A��A�A|�AG�A&�Ar�A�A/A�A�-A?}A�A
ĜA
v�A	�
A	;dA��A�RAbA�hAbNA��AA\)A�/A  AO�A �\@�=q@��@���@��@��@��R@���@�G�@�  @���@�b@�M�@�w@�-@�V@��m@�l�@�l�@�t�@�t�@�S�@�o@��#@�&�@�+@ى7@؋D@�b@�+@�J@ӕ�@�ff@�{@��#@� �@·+@���@�&�@̼j@�(�@�S�@�{@���@ǝ�@�33@Ƨ�@���@�x�@�V@ļj@�1'@��;@å�@�"�@�5?@�`B@�&�@���@�Z@��@�X@���@�9X@���@���@��!@�{@��-@�G�@��@��j@�Z@��@��y@�=q@���@�/@���@��@��@���@��-@�`B@�O�@��@��@���@�\)@��@�5?@��^@��@�/@��j@� �@��F@���@�1@���@��
@�ƨ@��F@��@�33@��@���@�V@�`B@���@�1'@�ƨ@�S�@�+@�n�@�$�@�@�/@���@�r�@� �@���@��F@�"�@���@�=q@��@�`B@�7L@��7@���@�x�@�G�@�/@�&�@�&�@�V@�V@��@��@�V@�V@�%@���@��`@��u@�A�@�b@���@���@�33@��@�ȴ@��!@�-@��@�^5@�$�@��-@��h@�V@���@�z�@�  @��F@�|�@���@�|�@��@���@��\@�n�@�^5@�E�@��@��@��h@�`B@�O�@��@��@�Ĝ@�Q�@�(�@�1@���@�;d@��H@���@�v�@�ff@�=q@��T@�@���@��7@�x�@�G�@�%@��@��u@�Z@� �@��
@��P@�t�@�\)@�;d@�"�@��y@��!@�V@�-@���@���@��^@��h@�X@���@�z�@�I�@�  @���@���@��w@�t�@��@���@�M�@���@�x�@���@���@�7L@�%@���@��9@�j@�A�@�1'@��@�t�@�;d@��@���@��+@�n�@�-@��@���@�G�@�/@��@�1'@��m@��
@��F@�K�@��@��y@���@�n�@�E�@���@��-@��7@�hs@�7L@��@��@��u@�r�@�Q�@�I�@�1'@�b@�1@���@��P@��P@�|�@�t�@�t�@�S�@��y@��@�ȴ@��\@�M�@��@��@��T@���@�@���@��h@���@���@�I�@��@l�@+@~�@~�+@~�+@~E�@}��@}�-@}p�@|�@|(�@{��@{��@{��@{"�@z~�@y��@y�7@y��@y�^@y&�@x��@x�@xbN@x1'@x1'@w��@v��@v$�@u�-@u/@t�/@t��@tz�@tz�@tz�@tz�@tI�@sƨ@s33@r�H@r~�@q�#@qG�@p�`@p�9@pr�@p1'@o��@o;d@o
=@nȴ@nv�@n$�@m�T@m�@mV@lj@k�m@kC�@j�\@i��@ihs@i&�@h�@g�@g��@g\)@f�y@f��@fv�@fv�@f{@e@eO�@d�D@d9X@c�F@ct�@cS�@cC�@co@b�!@bM�@bJ@a�#@a��@ax�@aX@`��@`Q�@`b@_l�@_+@^��@^�+@^$�@]@\�/@\�@\��@\z�@\�@[�m@[ƨ@[��@[33@Z��@Z^5@Z-@ZJ@Y��@YG�@X��@X�9@X��@X�u@Xr�@W�@W��@W�@Vȴ@Vff@V@U��@U�h@UO�@T�@Tj@T9X@T(�@T1@S�m@S�m@S��@SdZ@S33@R�H@R~�@Q��@Q�@Qhs@P�`@Pr�@O�w@OK�@Nȴ@N��@Nff@M�@M�-@M�@L��@L�@Lj@LZ@L9X@K�
@K��@KdZ@K33@J�@J��@J�!@Jn�@I�#@I�^@Ix�@IG�@H��@HĜ@Hb@G�P@F��@F�R@F�+@FV@F{@E�T@E��@E/@DZ@D(�@Cƨ@C��@C��@CS�@Co@B��@B^5@B�@A��@A�@A�#@A��@AG�@@��@@Ĝ@@�u@@A�@?�;@?�@?|�@?l�@?K�@?�@>��@>�@>E�@>@=p�@<�@<��@<z�@<Z@<�@;ƨ@;C�@:�\@:~�@:~�@:n�@:n�@:M�@9�#@9&�@8�9@8�@8A�@7�;@7�@7|�@7+@6�y@6��@6��@6ff@5�@5`B@5/@4��@4��@41@3��@3��@3�@333@2�!@2^5@2M�@2=q@2-@2J@2J@1��@1��@1x�@1X@17L@0�`@0r�@0  @/�@/l�@/;d@/+@.�@.v�@.E�@-�@-�T@-��@-�@-O�@,�@,z�@,j@,(�@+�m@+�F@+t�@+33@+"�@+o@*�!@*M�@*�@)�@)��@)��@)�7@)x�@)G�@(�`@(Ĝ@(�9@(��@(�@(r�@(A�@'�;@'|�@'\)@'K�@'+@&�@&�+@&v�@&5?@%�T@%�-@%�@%p�@%`B@%O�@%O�@$�@$�D@$Z@$1@#��@#�
@#�@#C�@"�@"��@"�\@"n�@"-@"�@!�@!�#@!�#@!�#@!��@!��@!�^@!hs@!7L@ ��@ ��@ �9@ ��@ �u@ r�@ Q�@ A�@  �@�;@�P@K�@�@��@�@v�@ff@ff@V@V@E�@$�@@��@�h@�h@�@p�@`B@?}@�@��@�D@�@�
@�F@��@��@��@��@�@C�@33@o@@�H@��@��@~�@n�@^5@M�@J@��@��@�#@X@7L@&�@��@�`@��@�9@bN@1'@ �@�;@�P@|�@l�@+@�y@��@v�@v�@ff@V@5?@{@�T@�-@p�@V@�@�/@�j@j@I�@9X@9X@��@ƨ@ƨ@ƨ@��@�@dZ@33@�@�!@^5@J@�#@��@�^@x�@x�@hs@G�@7L@&�@�@��@��@Ĝ@�u@Q�@1'@�@�@�P@l�@\)@
=1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A��A��/A��/A��/A��/A��;A��HA��HA��HA��TA��`A��`A��mA��mA��mA��`A��`A��mA��yA��yA��yA��yA��yA��A��yA��yA��yA��`A��#A���A���A�ȴA̸RA̗�A�x�A�C�A��A���A˼jA�1Aʗ�A�K�A���A�5?A��A�A���A�(�A��A�
=A��#A���A�ȴA���A��A�1A���A�9XA�oA�ƨA��A�ĜA�r�A�Q�A���A�VA��9A��^A�G�A��A��\A�ȴA�S�A���A�v�A�5?A�x�A�S�A�O�A��A���A�A��A��/A�bNA�bNA�I�A�|�A�z�A�A�A�A��A��A�XA��A��9A��A�ĜA�XA�  A��A��A�JA�;dA��uA�M�A���A�I�A�`BA��A�l�A��HA�1'A�33A�bA}�A|�`A|=qAy?}Ax1Av1'Ast�Ap��Am�^Ak�^Ait�Ag�AdjA_�wA^  A[�AZZAX�+AV�ATȴAR(�AQ�PAP��APQ�AN�\AMx�AK�hAI/AHffAFz�AE��AE�AD�`AD�DAD^5ADE�ADbABz�A>(�A<�A<I�A;l�A9`BA7%A6r�A6JA5��A5XA4�uA3��A3%A1K�A/�A-��A+�hA*�A*A�A)�-A(bNA'%A&ffA$VA"n�A!?}A ĜA��AVA  A�AA�+A�-A`BA�A��A�RA(�A�AhsA�A��A��A��A�A|�AG�A&�Ar�A�A/A�A�-A?}A�A
ĜA
v�A	�
A	;dA��A�RAbA�hAbNA��AA\)A�/A  AO�A �\@�=q@��@���@��@��@��R@���@�G�@�  @���@�b@�M�@�w@�-@�V@��m@�l�@�l�@�t�@�t�@�S�@�o@��#@�&�@�+@ى7@؋D@�b@�+@�J@ӕ�@�ff@�{@��#@� �@·+@���@�&�@̼j@�(�@�S�@�{@���@ǝ�@�33@Ƨ�@���@�x�@�V@ļj@�1'@��;@å�@�"�@�5?@�`B@�&�@���@�Z@��@�X@���@�9X@���@���@��!@�{@��-@�G�@��@��j@�Z@��@��y@�=q@���@�/@���@��@��@���@��-@�`B@�O�@��@��@���@�\)@��@�5?@��^@��@�/@��j@� �@��F@���@�1@���@��
@�ƨ@��F@��@�33@��@���@�V@�`B@���@�1'@�ƨ@�S�@�+@�n�@�$�@�@�/@���@�r�@� �@���@��F@�"�@���@�=q@��@�`B@�7L@��7@���@�x�@�G�@�/@�&�@�&�@�V@�V@��@��@�V@�V@�%@���@��`@��u@�A�@�b@���@���@�33@��@�ȴ@��!@�-@��@�^5@�$�@��-@��h@�V@���@�z�@�  @��F@�|�@���@�|�@��@���@��\@�n�@�^5@�E�@��@��@��h@�`B@�O�@��@��@�Ĝ@�Q�@�(�@�1@���@�;d@��H@���@�v�@�ff@�=q@��T@�@���@��7@�x�@�G�@�%@��@��u@�Z@� �@��
@��P@�t�@�\)@�;d@�"�@��y@��!@�V@�-@���@���@��^@��h@�X@���@�z�@�I�@�  @���@���@��w@�t�@��@���@�M�@���@�x�@���@���@�7L@�%@���@��9@�j@�A�@�1'@��@�t�@�;d@��@���@��+@�n�@�-@��@���@�G�@�/@��@�1'@��m@��
@��F@�K�@��@��y@���@�n�@�E�@���@��-@��7@�hs@�7L@��@��@��u@�r�@�Q�@�I�@�1'@�b@�1@���@��P@��P@�|�@�t�@�t�@�S�@��y@��@�ȴ@��\@�M�@��@��@��T@���@�@���@��h@���@���@�I�@��@l�@+@~�@~�+@~�+@~E�@}��@}�-@}p�@|�@|(�@{��@{��@{��@{"�@z~�@y��@y�7@y��@y�^@y&�@x��@x�@xbN@x1'@x1'@w��@v��@v$�@u�-@u/@t�/@t��@tz�@tz�@tz�@tz�@tI�@sƨ@s33@r�H@r~�@q�#@qG�@p�`@p�9@pr�@p1'@o��@o;d@o
=@nȴ@nv�@n$�@m�T@m�@mV@lj@k�m@kC�@j�\@i��@ihs@i&�@h�@g�@g��@g\)@f�y@f��@fv�@fv�@f{@e@eO�@d�D@d9X@c�F@ct�@cS�@cC�@co@b�!@bM�@bJ@a�#@a��@ax�@aX@`��@`Q�@`b@_l�@_+@^��@^�+@^$�@]@\�/@\�@\��@\z�@\�@[�m@[ƨ@[��@[33@Z��@Z^5@Z-@ZJ@Y��@YG�@X��@X�9@X��@X�u@Xr�@W�@W��@W�@Vȴ@Vff@V@U��@U�h@UO�@T�@Tj@T9X@T(�@T1@S�m@S�m@S��@SdZ@S33@R�H@R~�@Q��@Q�@Qhs@P�`@Pr�@O�w@OK�@Nȴ@N��@Nff@M�@M�-@M�@L��@L�@Lj@LZ@L9X@K�
@K��@KdZ@K33@J�@J��@J�!@Jn�@I�#@I�^@Ix�@IG�@H��@HĜ@Hb@G�P@F��@F�R@F�+@FV@F{@E�T@E��@E/@DZ@D(�@Cƨ@C��@C��@CS�@Co@B��@B^5@B�@A��@A�@A�#@A��@AG�@@��@@Ĝ@@�u@@A�@?�;@?�@?|�@?l�@?K�@?�@>��@>�@>E�@>@=p�@<�@<��@<z�@<Z@<�@;ƨ@;C�@:�\@:~�@:~�@:n�@:n�@:M�@9�#@9&�@8�9@8�@8A�@7�;@7�@7|�@7+@6�y@6��@6��@6ff@5�@5`B@5/@4��@4��@41@3��@3��@3�@333@2�!@2^5@2M�@2=q@2-@2J@2J@1��@1��@1x�@1X@17L@0�`@0r�@0  @/�@/l�@/;d@/+@.�@.v�@.E�@-�@-�T@-��@-�@-O�@,�@,z�@,j@,(�@+�m@+�F@+t�@+33@+"�@+o@*�!@*M�@*�@)�@)��@)��@)�7@)x�@)G�@(�`@(Ĝ@(�9@(��@(�@(r�@(A�@'�;@'|�@'\)@'K�@'+@&�@&�+@&v�@&5?@%�T@%�-@%�@%p�@%`B@%O�@%O�@$�@$�D@$Z@$1@#��@#�
@#�@#C�@"�@"��@"�\@"n�@"-@"�@!�@!�#@!�#@!�#@!��@!��@!�^@!hs@!7L@ ��@ ��@ �9@ ��@ �u@ r�@ Q�@ A�@  �@�;@�P@K�@�@��@�@v�@ff@ff@V@V@E�@$�@@��@�h@�h@�@p�@`B@?}@�@��@�D@�@�
@�F@��@��@��@��@�@C�@33@o@@�H@��@��@~�@n�@^5@M�@J@��@��@�#@X@7L@&�@��@�`@��@�9@bN@1'@ �@�;@�P@|�@l�@+@�y@��@v�@v�@ff@V@5?@{@�T@�-@p�@V@�@�/@�j@j@I�@9X@9X@��@ƨ@ƨ@ƨ@��@�@dZ@33@�@�!@^5@J@�#@��@�^@x�@x�@hs@G�@7L@&�@�@��@��@Ĝ@�u@Q�@1'@�@�@�P@l�@\)@
=1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BPBJBJBJBJBJBJBJBJBPBPBPBPBPBPBPBPBPBPBPBPBVBPBVBPBVBVBVB\BoB{B�B�B�B"�B(�B1'B33B2-B1'BD�B>wB7LBM�B^5B\)BS�B>wB/B9XBH�BP�BO�BVBR�Bk�Bn�BffBgmBn�Bs�B�B�JB�bB�1B{�B�7B{�Bv�Bv�Bn�Bn�BiyBcTB_;BZBM�B?}B5?B;dB0!B�B�B��B�yB�BBǮB�qB�B��B}�BdZBB�B  B
�B
�BB
��B
�\B
��B
�oB
{�B
R�B
T�B
S�B
N�B
ZB
YB
�B
��B
�bB
��B
�hB
�%B
q�B
_;B
P�B
M�B
H�B
/B
 �B
bB	�B	�#B	��B	�3B	��B	�=B	o�B	<jB	L�B	@�B	6FB	0!B	�B	�B	\B	�B	�B	�B	B	  B�B�/B�B�5B�B�B�B�B�B�sB�/BƨB��B�wBĜB�wB�B��B�9B�LB�?B�3B�B��B��B�\B�B�B}�B�JB�1B�%B{�Bp�Bv�Be`BcTBffBp�BgmBe`B_;B^5BZBYB^5Be`BbNBXBQ�BaHB_;BbNB]/BN�BK�BB�BH�BP�BXB]/BXBR�B]/BZBQ�B\)B_;B[#BXBP�BL�B?}BH�BN�BL�BH�BM�BVBP�BH�BG�BD�BE�B?}BD�BJ�BF�BH�BS�BVBYBP�BG�BF�B)�B0!BN�BZBW
B]/BaHBbNBaHB`BB]/BW
BYBS�BS�B_;BbNB_;B\)BXBcTBk�BiyBe`BgmBp�Bq�Bt�Bt�Bq�Bp�Bt�By�B�B�B�B�PB�VB�\B�bB�uB�uB�hB�bB�uB��B��B��B�bB��B��B�B�B�B�B�-B�LB�XB�dB�qB�jB�dB�}BÖBȴB��B��B��B�B�/B�;B�yB�B�B�B�B��B��B��B	%B	JB	VB	bB	{B	�B	%�B	(�B	)�B	+B	,B	-B	,B	,B	0!B	49B	49B	2-B	8RB	;dB	=qB	>wB	A�B	A�B	F�B	G�B	I�B	I�B	N�B	N�B	P�B	S�B	R�B	W
B	^5B	bNB	dZB	k�B	p�B	y�B	}�B	�B	�B	�B	�B	�+B	�=B	�DB	�PB	�\B	�hB	�uB	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�?B	�XB	�dB	�dB	�wB	��B	ÖB	ĜB	ĜB	ĜB	ĜB	ĜB	ǮB	��B	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�
B	�B	�#B	�#B	�#B	�B	�B	�#B	�5B	�5B	�;B	�;B	�BB	�TB	�ZB	�ZB	�ZB	�TB	�TB	�TB	�`B	�`B	�fB	�mB	�fB	�fB	�ZB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
%B
+B
1B
1B
1B
1B

=B

=B

=B
DB

=B
JB
PB
VB
VB
VB
VB
VB
\B
bB
oB
uB
{B
{B
{B
oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
!�B
�B
�B
�B
�B
�B
 �B
!�B
"�B
$�B
#�B
$�B
#�B
#�B
#�B
$�B
$�B
#�B
$�B
%�B
&�B
&�B
&�B
&�B
'�B
(�B
)�B
(�B
)�B
)�B
)�B
(�B
(�B
)�B
)�B
(�B
)�B
+B
,B
+B
+B
-B
,B
-B
.B
/B
/B
.B
.B
-B
-B
.B
.B
0!B
0!B
0!B
0!B
/B
0!B
1'B
1'B
1'B
1'B
1'B
0!B
/B
2-B
1'B
2-B
2-B
1'B
2-B
2-B
1'B
49B
5?B
49B
49B
49B
5?B
49B
33B
33B
49B
5?B
5?B
49B
5?B
5?B
6FB
7LB
7LB
6FB
5?B
5?B
5?B
6FB
6FB
7LB
8RB
7LB
7LB
8RB
7LB
9XB
:^B
:^B
9XB
9XB
9XB
9XB
9XB
9XB
8RB
8RB
:^B
9XB
9XB
9XB
8RB
:^B
:^B
;dB
<jB
;dB
<jB
=qB
;dB
<jB
=qB
>wB
=qB
<jB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
=qB
?}B
?}B
?}B
?}B
@�B
?}B
@�B
A�B
B�B
C�B
C�B
B�B
C�B
B�B
B�B
A�B
D�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
F�B
F�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
F�B
G�B
G�B
H�B
I�B
J�B
J�B
J�B
J�B
I�B
J�B
M�B
M�B
M�B
M�B
L�B
K�B
K�B
L�B
N�B
N�B
M�B
O�B
O�B
O�B
O�B
O�B
P�B
O�B
O�B
O�B
Q�B
Q�B
Q�B
Q�B
Q�B
T�B
S�B
S�B
R�B
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
VB
VB
VB
VB
VB
VB
VB
W
B
XB
YB
YB
XB
XB
YB
YB
ZB
ZB
ZB
ZB
YB
ZB
[#B
[#B
[#B
\)B
[#B
\)B
]/B
\)B
\)B
[#B
\)B
\)B
]/B
^5B
^5B
^5B
]/B
]/B
_;B
_;B
_;B
_;B
_;B
^5B
^5B
^5B
_;B
`BB
_;B
_;B
_;B
`BB
`BB
`BB
aHB
aHB
bNB
bNB
bNB
bNB
aHB
aHB
bNB
bNB
cTB
cTB
bNB
cTB
cTB
dZB
dZB
e`B
dZB
e`B
e`B
ffB
ffB
ffB
ffB
ffB
e`B
dZB
e`B
e`B
ffB
ffB
ffB
ffB
ffB
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
iyB
iyB
iyB
iyB
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
hsB
hsB
hsB
jB
k�B
k�B
k�B
k�B
k�B
k�B
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
k�B
l�B
l�B
l�B
jB
m�B
m�B
l�B
m�B
m�B
m�B
l�B
m�B
n�B
m�B
m�B
n�B
n�B
m�B
n�B
n�B
o�B
p�B
p�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
q�B
p�B
p�B
q�B
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
r�B
r�B
r�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
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
v�B
v�B
v�B
w�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BPBJBJBJBJBJBJBJBJBPBPBPBjBjBPBPBPBPBPBPBPBVBPBVBjBVBVBpB�B�B�B�B�B�B# B)yB1vB3�B2�B2�BFYBA�B;�BOBB^�B\�BUgBB�B4B<�BKDBS@BR BXBU�Bl�Bp;BiyBjBqvBvFB�gB�B�4B�	B~�B��B}�BxBxRBp�Bp!BjBdtB`'B[	BO�BA�B7fB<jB1vB�B�B�]B��B�B�XB��B��B�\B�'Bh>BG�B	B
�B
�:B
�IB
��B
��B
��B
~�B
W�B
WYB
U�B
PbB
Z�B
ZkB
��B
�bG�O�B
��B
��B
�1B
tTB
bB
SuB
OBB
J#B
2aB
"�B
B	�+B	޸B	�B	��B	��B	�B	tB	BB	N�B	CB	8�B	2|B	"�B	!B	 B	 �B	�B	�B	-B	�B�'B�BB�B��B�qB�/B��B�B��B��B�jB�rG�O�B�OBňB� B��B��B��B�B��B��B�)B�B�'B��B��B��B�iB�PB�7B�EB}�Br�Bx8Bh>Be�Bh
Bq�Bh�Bf�B`�B_�B[�BZ�B_pBe�Bc BY�BS�Ba�B_�Bb�B^BP�BMPBD�BI�BQ�BXyB]�BY1BTFB]�BZ�BS@B\�B_�B[�BX�BQ�BNBA�BI�BO�BM�BJXBN�BVSBQ�BI�BH�BE�BF�BAUBE�BK�BHBJ#BT�BV�BY�BR BI�BH�B.IB4BO�BZ�BW�B]�Ba|Bb�BabB`�B]�BXBY�BUgBUMB`Bb�B`'B]/BY�Bd&Bk�BjBf�Bh�Bq'Br-Bu?BuZBraBq�Bu�Bz�B��B��B��B��B��B��B��B��B��B��B�4B�,B��B�B�_B� B�&B�B�kB��B��B��B��B��B��B��B��B��B�B�4B�3B�7B�JB�\B��B�yBݲB��B�B��B��B�)B�;B�B�<B��B	�B	~B	�B	�B	�B	�B	%�B	)B	*B	+6B	,"B	-)B	,=B	,WB	0�B	4�B	4�B	2�B	8�B	;�B	=�B	>�B	A�B	BB	F�B	HB	J=B	J#B	O(B	O(B	Q4B	TFB	SuB	WsB	^�B	b�B	d�B	k�B	p�B	y�B	~(B	�;B	�-B	�MB	�SB	�_B	�XB	�DB	�PB	�vB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�B	��B	��B	�$B	�FB	�>B	�_B	�=B	�kB	�iB	�vB	�tB	�rB	��B	��B	��B	��B	ðB	ĶB	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	��B	�(B	�"B	�.B	�&B	�2B	�EB	�EB	�EB	�sB	�QB	�=B	�=B	�=B	�QB	�kB	�qB	�OB	�jB	�pB	�pB	��B	�nB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	�B	��B	��B	�B	�B	��B	� B	��B	��B	�-B	��B	��B	�B	�%B	�B	�B	��B	�B	�B	�	B	�LB	�B	�B	�0B	�"B	�B	�(B	�"B	�<B	�"B	�6B	�B	�PB	�cB
UB
GB
aB
[B
SB
tB
zB
fB
�B
�B
�B

XB

rB

�B
xB

�B
dB
�B
�B
�B
pB
pB
pB
�B
�B
oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�G�O�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 B
 B
!�B
!�B
!�B
!�B
 B
!B
B
�B
 B
 �B
!�B
"�B
$�B
#�B
$�B
#�B
$B
$&B
$�B
%B
$B
%B
%�B
'B
'B
'B
'8B
($B
)*B
*B
)B
*0B
*B
*0B
)*B
)_B
*KB
*KB
)DB
*eB
+QB
,=B
+QB
+QB
-CB
,WB
-CB
./B
/OB
/OB
.cB
.IB
-CB
-wB
.IB
.IB
0UB
0;B
0UB
0UB
/OB
0;B
1AB
1AB
1AB
1AB
1AB
0oB
/�B
2GB
1vB
2aB
2aB
1[B
2|B
2aB
1vB
4nB
5ZB
4TB
4nB
4nB
5tB
4nB
3�B
3�B
4TB
5tB
5tB
4nB
5ZB
5tB
6`B
7fB
7�B
6zB
5tB
5tB
5�B
6zB
6zB
7fB
8lB
7�B
7�B
8�B
7�B
9rB
:xB
:xB
9�B
9rB
9rB
9rB
9rB
9rB
8�B
8�B
:�B
9�B
9�B
9�B
8�B
:�B
:�B
;B
<�B
;�B
<�B
=�B
;�B
<�B
=�B
>�B
=�B
<�B
>�B
>�B
>�B
>�B
>�B
>�B
>�B
=�B
?�B
?�B
?�B
?�B
@�B
?�B
@�B
A�B
B�B
C�B
C�B
B�B
C�B
B�B
B�B
A�B
D�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
F�B
F�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
G�B
H�B
H�B
H�B
H�B
H�B
H�G�O�B
G�B
G�B
H�B
I�B
J�B
J�B
J�B
J�B
J	B
KB
M�B
M�B
M�B
M�B
L�B
LB
LB
MB
N�B
N�B
NB
O�B
O�B
O�B
PB
O�B
Q B
PB
P.B
P.B
R B
R B
R:B
R B
R:B
UB
T,B
TB
S&B
UB
VB
VB
W
B
VB
W
B
W?B
V9B
V9B
VB
VB
V9B
V9B
V9B
W?B
X+B
YKB
YKB
X+B
XEB
YKB
YKB
Z7B
ZQB
ZQB
ZQB
YKB
Z7B
[WB
[=B
[=B
\]B
[WB
\CB
]IB
\]B
\CB
[WB
\]B
\]B
]dB
^OB
^OB
^OB
]dB
]dB
_VB
_VB
_;B
_pB
_VB
^jB
^jB
^OB
_VB
`BB
_VB
_VB
_pB
`\B
`vB
`\B
a|B
a|B
bhB
bNB
bhB
bNB
a|B
abB
bhB
bhB
cnB
cnB
b�B
cnB
c�B
dtB
dtB
e�B
dtB
ezB
e�B
ffB
f�B
f�B
f�B
ffB
ezB
d�B
ezB
ezB
f�B
f�B
ffB
f�B
f�B
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
iyB
i�B
i�B
iyB
h�B
h�B
h�B
h�B
h�B
iyB
iyB
i�B
iyB
i�B
i�B
h�B
h�B
h�B
j�B
k�B
k�B
k�B
k�B
k�B
k�B
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
k�B
l�B
l�B
l�B
j�B
m�B
m�B
l�B
m�B
m�B
m�B
l�B
m�B
n�B
m�B
m�B
n�B
n�B
m�B
n�B
n�B
o�B
p�B
p�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
q�B
p�B
p�B
q�B
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
r�B
r�B
r�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
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
v�B
v�B
v�B
w�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.11(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201712100036372017121000363720171210003637201806221322592018062213225920180622132259201804050726042018040507260420180405072604  JA  ARFMdecpA19c                                                                20171206093519  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20171206003527  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20171206003530  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20171206003530  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20171206003531  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20171206003531  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20171206003531  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20171206003531  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20171206003531  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20171206003531                      G�O�G�O�G�O�                JA  ARUP                                                                        20171206005548                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20171206153318  CV  JULD            G�O�G�O�F�ծ                JM  ARSQJMQC2.0                                                                 20171207000000  CF  PSAL_ADJUSTED_QCC]�fD�� G�O�                JM  ARCAJMQC2.0                                                                 20171209153637  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20171209153637  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404222604  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622042259  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116221515                      G�O�G�O�G�O�                