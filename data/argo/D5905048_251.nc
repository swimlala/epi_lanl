CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-06-16T00:35:13Z creation;2018-06-16T00:35:17Z conversion to V3.1;2019-12-19T07:35:49Z update;     
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
_FillValue                 �  I@   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p`   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tL   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �l   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �x   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �x   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �x   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �x   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �H   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �X   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �\   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �l   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �p   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �t   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �xArgo profile    3.1 1.2 19500101000000  20180616003513  20200116231515  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0577_251                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @�j��u� 1   @�j�l��@4`�d���dQW>�6z1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @333@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  A�33B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D	fD	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{�fD|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�9�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @,(�@x��@�z�@�z�A=qA>=qA^=qA~=qA��A��A��A��A��A��A��A�Q�B�\B�\B�\B�\B'�\B/�\B7�\B?�\BG�\BO�\BW�\B_�\Bg�\Bo�\Bw�\B�\B�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮC��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D x�D ��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D�\Dx�D�\D	x�D	��D
x�D
��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��D x�D ��D!x�D!��D"x�D"��D#x�D#��D$x�D$��D%x�D%��D&x�D&��D'x�D'��D(x�D(��D)x�D)��D*x�D*��D+x�D+��D,x�D,��D-x�D-��D.x�D.��D/x�D/��D0x�D0��D1x�D1��D2x�D2��D3x�D3��D4x�D4��D5x�D5��D6x�D6��D7x�D7��D8x�D8��D9x�D9��D:x�D:��D;x�D;��D<x�D<��D=x�D=��D>x�D>��D?x�D?��D@x�D@��DAx�DA��DBx�DB��DCx�DC��DDx�DD��DEx�DE��DFx�DF��DGx�DG��DHx�DH��DIx�DI��DJx�DJ��DKx�DK��DLx�DL��DMx�DM��DNx�DN��DOx�DO��DPx�DP��DQx�DQ��DRx�DR��DSx�DS��DTx�DT��DUx�DU��DVx�DV��DWx�DW��DXx�DX��DYx�DY��DZx�DZ��D[x�D[��D\x�D\��D]x�D]��D^x�D^��D_x�D_��D`x�D`��Dax�Da��Dbx�Db��Dcx�Dc��Ddx�Dd��Dex�De��Dfx�Df��Dgx�Dg��Dhx�Dh��Dix�Di��Djx�Dj��Dkx�Dk��Dlx�Dl��Dmx�Dm��Dnx�Dn��Dox�Do��Dpx�Dp��Dqx�Dq��Drx�Dr��Dsx�Ds��Dtx�Dt��Dux�Du��Dvx�Dv��Dwx�Dw��Dxx�Dx��Dyx�Dy��Dzx�Dz��D{\D{��D|x�D|��D}x�D}��D~x�D~��Dx�D��D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D¼{D��{D�<{D�|{Dü{D��{D�<{D�|{Dļ{D��{D�<{D�|{Dż{D��{D�<{D�|{DƼ{D��{D�<{D�|{DǼ{D��{D�<{D�|{Dȼ{D��{D�<{D�|{Dɼ{D��{D�<{D�|{Dʼ{D��{D�<{D�|{D˼{D��{D�<{D�|{D̼{D��{D�<{D�|{Dͼ{D��{D�<{D�|{Dμ{D��{D�<{D�|{Dϼ{D��{D�<{D�|{Dм{D��{D�<{D�|{DѼ{D��{D�<{D�|{DҼ{D��{D�<{D�|{DӼ{D��{D�<{D�|{DԼ{D��{D�<{D�|{Dռ{D��{D�<{D�|{Dּ{D��{D�<{D�|{D׼{D��{D�<{D�|{Dؼ{D��{D�<{D�|{Dټ{D��{D�<{D�|{Dڼ{D��{D�<{D�|{Dۼ{D��{D�<{D�|{Dܼ{D��{D�<{D�|{Dݼ{D��{D�<{D�|{D޼{D��{D�<{D�|{D߼{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�9HD�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D��D�{D��HD�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D��{D���D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D���D�611111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A��#A���AҶFAҮAэPAϾwA�
=Aκ^A΋DA�n�A�dZA�O�A��A���A���A�AˮAȝ�A�bA�`BA��A�t�AċDA��
A�JA®A�=qA�bNA�
=A�r�A�ƨA�+A���A�A�A��A��A�/A�  A���A���A�+A���A���A��A�\)A���A�A�1'A��-A��uA�t�A�Q�A�{A���A�A�%A�bNA���A�ffA�+A�?}A��PA���A��A�hsA� �A���A��A�(�A�hsA���A��DA��TA��A��jA�5?A��uA��^A���A���A���A�M�A�9XA��jA�O�A���A��A���A�bA��-A�ĜA�O�A��HA�ƨA�1'A��yA��yA�$�A��RA�p�A~�+Az��Av�Ar=qAp��Am"�Aj��Ah�!Agl�Ae��Ac�#A`��A]��A\�A[�#AZ�!AY�hAW�
AU�AQ��AP{AO�^AN�HAN1'AK�AJ��AIƨAI33AH��AHz�AG��AF�/AEt�ACO�AB�`AB�!ABr�AA��A?33A=A<1A:  A7�PA6�A45?A2��A0�uA/��A.��A-�PA,I�A)�;A(I�A'�A&A�A%t�A$��A$��A$A�A#33A"n�A VA   A�#AA��A�A�7AĜAVAE�A  A`BA�AȴA��A=qA5?AXA�A�jA�A;dAAĜA��A�;A
�HA
�!A
{A	��A	O�An�A�A33Az�A;dA�A$�A��A�mAXA r�@�|�@�-@���@�@�?}@�z�@�b@��@�-@���@�P@�v�@�1'@��#@�Q�@�t�@�@��@��@��/@��@��T@�z�@��@ߥ�@߅@�dZ@�V@�/@ڇ+@���@٩�@��@��m@�^5@���@� �@�@�@��T@љ�@�t�@ͺ^@��@̛�@�j@˾w@�@�z�@�S�@�ff@Ų-@��@Ĭ@�1'@Ý�@�"�@�M�@���@��^@���@��h@�x�@�1'@���@�~�@���@�~�@�hs@��@��
@��@��@��@��!@��@��/@���@��m@��!@��@���@�7L@�7L@��@��D@�b@��
@��@�K�@�
=@�V@���@��@���@�I�@�ƨ@��P@�C�@�"�@��H@�n�@�J@�p�@��@��@�j@�A�@��m@���@�K�@��@��H@�v�@�-@���@�p�@�/@���@��9@��u@��@�(�@���@�|�@�K�@�33@��@�
=@��H@��R@���@�~�@�=q@��T@�`B@��/@��u@�r�@�Z@� �@�b@��m@�|�@�33@��@��H@�n�@�5?@�J@���@��@��^@�x�@�O�@�/@��j@�Z@�A�@��w@�;d@�@���@���@�=q@�@��@�V@���@���@��j@�z�@�(�@���@���@�S�@���@�ȴ@��R@���@�~�@�n�@�M�@��@��@��7@�&�@���@�z�@�  @�ƨ@���@�;d@�
=@��H@��H@��@���@�=q@��#@�p�@�&�@��j@��u@�Z@� �@��
@��@�dZ@�
=@���@���@��+@�v�@�ff@�^5@�-@��#@���@��-@���@�O�@��/@�bN@� �@���@�ƨ@�|�@�dZ@�\)@�\)@�C�@�o@��H@��\@�^5@�5?@���@���@�?}@�7L@�7L@��@���@�z�@�9X@��@�  @���@��P@�K�@�K�@�l�@�@��R@�=q@�{@���@�@���@�`B@�G�@�7L@��j@��j@���@���@�Z@��D@�I�@���@�t�@�"�@�@��y@���@��@�o@��+@�n�@�^5@�E�@��@��@��@�@��@��T@�@�x�@�&�@���@���@��9@��D@�r�@�bN@�Z@�r�@�9X@��@�K�@�+@�@��@���@���@�V@��@���@�G�@�?}@��@�V@��`@��@�j@�bN@�Z@�1'@� �@��@�  @�  @�  @;d@~�+@}�@}O�@}V@|��@|I�@{��@{t�@z�@z�!@z��@z��@z��@z��@z��@y�#@yx�@y�@xbN@x1'@w�@w�w@w�P@w�P@w�P@w�P@w;d@v�y@v�@v�R@vv�@u�T@up�@u�@t��@t�@t�@s��@so@r^5@q�7@p��@pĜ@p�u@pbN@pA�@p �@p  @o��@o\)@o+@n��@m�T@mp�@m?}@l�j@lZ@l9X@l�@k��@k@jn�@j-@i�#@iX@h�`@h1'@g��@g
=@f$�@eO�@d�D@cdZ@co@b�!@b^5@a�#@a�7@aX@`��@`�@`b@_�P@_+@^�@^�+@^5?@]�@]�-@]p�@]�@\�D@\�@[t�@[33@Z�H@ZM�@Y�^@Y7L@Y%@X��@X�9@XA�@Xb@W�@W|�@W
=@Vȴ@V�R@V�R@V��@V��@V��@V�+@Vff@VV@U��@T��@TZ@T(�@T�@T1@S�m@SS�@R�H@R�\@R~�@RM�@Q��@Q��@Q�@Pr�@O�;@O|�@OK�@N�@N��@Nv�@N$�@N@M�h@M`B@MV@L�j@Lj@K�m@KC�@J�H@JM�@I�@I�#@I7L@H��@HbN@H �@G�@G�;@G��@G�@G|�@G�@F��@Fff@F@E�-@E�@EO�@E�@EV@D�@D��@D�D@C��@C�
@C�m@C�F@CS�@C"�@B�H@B��@B=q@B�@A��@A��@Ahs@@��@@b@?��@?l�@?
=@>v�@>$�@=�@=@=�@=V@<��@<�D@<j@<I�@<(�@<(�@<�@;�m@;�F@;�@;"�@:�!@:^5@:�@9�^@9��@9��@9��@9x�@97L@9�@8�u@7�@7|�@7\)@7+@6�@6��@6V@65?@6$�@6@5�T@5�-@5��@5�-@5��@5?}@5V@4�@49X@3�m@3�F@3��@3�@3C�@3"�@2��@2~�@2=q@1�@1�^@1x�@1�@0��@0��@0bN@0Q�@01'@/�@/\)@/
=@.�+@-�T@-��@-O�@,�@,��@,�j@,�@,�D@,I�@,1@+��@+ƨ@+��@+"�@*�@*�\@*�\@*~�@*M�@*J@)��@)�^@)��@)�7@)7L@(��@(Ĝ@(�@(bN@(b@'�@'�w@'l�@&��@&ȴ@&ȴ@&�R@&�R@&��@&�+@&E�@&{@%�@%�-@%p�@$�/@$�D@$I�@$�@$1@$1@#�
@#dZ@#S�@#C�@#o@"�H@"�!@"��@"^5@"M�@"-@!��@!�^@!�7@!G�@!&�@!%@ ��@ A�@ b@�@��@�w@�w@�@�@��@�P@|�@K�@+@�y@��@$�@�@��@`B@O�@/@�/@��@z�@z�@z�@z�@z�@z�@I�@1@ƨ@�@@��@=q@J@�#@��@x�@�@�`@Ĝ@�u@A�@b@�@�;@l�@+@+@��@�R@��@v�@5?@@�T@�-@O�@�/@�D@I�@1@�m@�
@ƨ@��@��@��@S�@o@�@�H@�!@n�@=q@J@�@��@�7@x�@�@�@�@�@��@�9@��@�u@�@Q�@b@�@�@�;@��@��@K�@�@��@�@�R@��@�+@5?@@�@�@��@@�-@��@p�@O�@�@��@��@�@�/@�j@��@z�@I�@�@�
@�F@��@t�@dZ@33@
�H@
��@
�\@
~�@
n�@
n�@
=q@	�@	�#@	��@	��@	��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A��#A���AҶFAҮAэPAϾwA�
=Aκ^A΋DA�n�A�dZA�O�A��A���A���A�AˮAȝ�A�bA�`BA��A�t�AċDA��
A�JA®A�=qA�bNA�
=A�r�A�ƨA�+A���A�A�A��A��A�/A�  A���A���A�+A���A���A��A�\)A���A�A�1'A��-A��uA�t�A�Q�A�{A���A�A�%A�bNA���A�ffA�+A�?}A��PA���A��A�hsA� �A���A��A�(�A�hsA���A��DA��TA��A��jA�5?A��uA��^A���A���A���A�M�A�9XA��jA�O�A���A��A���A�bA��-A�ĜA�O�A��HA�ƨA�1'A��yA��yA�$�A��RA�p�A~�+Az��Av�Ar=qAp��Am"�Aj��Ah�!Agl�Ae��Ac�#A`��A]��A\�A[�#AZ�!AY�hAW�
AU�AQ��AP{AO�^AN�HAN1'AK�AJ��AIƨAI33AH��AHz�AG��AF�/AEt�ACO�AB�`AB�!ABr�AA��A?33A=A<1A:  A7�PA6�A45?A2��A0�uA/��A.��A-�PA,I�A)�;A(I�A'�A&A�A%t�A$��A$��A$A�A#33A"n�A VA   A�#AA��A�A�7AĜAVAE�A  A`BA�AȴA��A=qA5?AXA�A�jA�A;dAAĜA��A�;A
�HA
�!A
{A	��A	O�An�A�A33Az�A;dA�A$�A��A�mAXA r�@�|�@�-@���@�@�?}@�z�@�b@��@�-@���@�P@�v�@�1'@��#@�Q�@�t�@�@��@��@��/@��@��T@�z�@��@ߥ�@߅@�dZ@�V@�/@ڇ+@���@٩�@��@��m@�^5@���@� �@�@�@��T@љ�@�t�@ͺ^@��@̛�@�j@˾w@�@�z�@�S�@�ff@Ų-@��@Ĭ@�1'@Ý�@�"�@�M�@���@��^@���@��h@�x�@�1'@���@�~�@���@�~�@�hs@��@��
@��@��@��@��!@��@��/@���@��m@��!@��@���@�7L@�7L@��@��D@�b@��
@��@�K�@�
=@�V@���@��@���@�I�@�ƨ@��P@�C�@�"�@��H@�n�@�J@�p�@��@��@�j@�A�@��m@���@�K�@��@��H@�v�@�-@���@�p�@�/@���@��9@��u@��@�(�@���@�|�@�K�@�33@��@�
=@��H@��R@���@�~�@�=q@��T@�`B@��/@��u@�r�@�Z@� �@�b@��m@�|�@�33@��@��H@�n�@�5?@�J@���@��@��^@�x�@�O�@�/@��j@�Z@�A�@��w@�;d@�@���@���@�=q@�@��@�V@���@���@��j@�z�@�(�@���@���@�S�@���@�ȴ@��R@���@�~�@�n�@�M�@��@��@��7@�&�@���@�z�@�  @�ƨ@���@�;d@�
=@��H@��H@��@���@�=q@��#@�p�@�&�@��j@��u@�Z@� �@��
@��@�dZ@�
=@���@���@��+@�v�@�ff@�^5@�-@��#@���@��-@���@�O�@��/@�bN@� �@���@�ƨ@�|�@�dZ@�\)@�\)@�C�@�o@��H@��\@�^5@�5?@���@���@�?}@�7L@�7L@��@���@�z�@�9X@��@�  @���@��P@�K�@�K�@�l�@�@��R@�=q@�{@���@�@���@�`B@�G�@�7L@��j@��j@���@���@�Z@��D@�I�@���@�t�@�"�@�@��y@���@��@�o@��+@�n�@�^5@�E�@��@��@��@�@��@��T@�@�x�@�&�@���@���@��9@��D@�r�@�bN@�Z@�r�@�9X@��@�K�@�+@�@��@���@���@�V@��@���@�G�@�?}@��@�V@��`@��@�j@�bN@�Z@�1'@� �@��@�  @�  @�  @;d@~�+@}�@}O�@}V@|��@|I�@{��@{t�@z�@z�!@z��@z��@z��@z��@z��@y�#@yx�@y�@xbN@x1'@w�@w�w@w�P@w�P@w�P@w�P@w;d@v�y@v�@v�R@vv�@u�T@up�@u�@t��@t�@t�@s��@so@r^5@q�7@p��@pĜ@p�u@pbN@pA�@p �@p  @o��@o\)@o+@n��@m�T@mp�@m?}@l�j@lZ@l9X@l�@k��@k@jn�@j-@i�#@iX@h�`@h1'@g��@g
=@f$�@eO�@d�D@cdZ@co@b�!@b^5@a�#@a�7@aX@`��@`�@`b@_�P@_+@^�@^�+@^5?@]�@]�-@]p�@]�@\�D@\�@[t�@[33@Z�H@ZM�@Y�^@Y7L@Y%@X��@X�9@XA�@Xb@W�@W|�@W
=@Vȴ@V�R@V�R@V��@V��@V��@V�+@Vff@VV@U��@T��@TZ@T(�@T�@T1@S�m@SS�@R�H@R�\@R~�@RM�@Q��@Q��@Q�@Pr�@O�;@O|�@OK�@N�@N��@Nv�@N$�@N@M�h@M`B@MV@L�j@Lj@K�m@KC�@J�H@JM�@I�@I�#@I7L@H��@HbN@H �@G�@G�;@G��@G�@G|�@G�@F��@Fff@F@E�-@E�@EO�@E�@EV@D�@D��@D�D@C��@C�
@C�m@C�F@CS�@C"�@B�H@B��@B=q@B�@A��@A��@Ahs@@��@@b@?��@?l�@?
=@>v�@>$�@=�@=@=�@=V@<��@<�D@<j@<I�@<(�@<(�@<�@;�m@;�F@;�@;"�@:�!@:^5@:�@9�^@9��@9��@9��@9x�@97L@9�@8�u@7�@7|�@7\)@7+@6�@6��@6V@65?@6$�@6@5�T@5�-@5��@5�-@5��@5?}@5V@4�@49X@3�m@3�F@3��@3�@3C�@3"�@2��@2~�@2=q@1�@1�^@1x�@1�@0��@0��@0bN@0Q�@01'@/�@/\)@/
=@.�+@-�T@-��@-O�@,�@,��@,�j@,�@,�D@,I�@,1@+��@+ƨ@+��@+"�@*�@*�\@*�\@*~�@*M�@*J@)��@)�^@)��@)�7@)7L@(��@(Ĝ@(�@(bN@(b@'�@'�w@'l�@&��@&ȴ@&ȴ@&�R@&�R@&��@&�+@&E�@&{@%�@%�-@%p�@$�/@$�D@$I�@$�@$1@$1@#�
@#dZ@#S�@#C�@#o@"�H@"�!@"��@"^5@"M�@"-@!��@!�^@!�7@!G�@!&�@!%@ ��@ A�@ b@�@��@�w@�w@�@�@��@�P@|�@K�@+@�y@��@$�@�@��@`B@O�@/@�/@��@z�@z�@z�@z�@z�@z�@I�@1@ƨ@�@@��@=q@J@�#@��@x�@�@�`@Ĝ@�u@A�@b@�@�;@l�@+@+@��@�R@��@v�@5?@@�T@�-@O�@�/@�D@I�@1@�m@�
@ƨ@��@��@��@S�@o@�@�H@�!@n�@=q@J@�@��@�7@x�@�@�@�@�@��@�9@��@�u@�@Q�@b@�@�@�;@��@��@K�@�@��@�@�R@��@�+@5?@@�@�@��@@�-@��@p�@O�@�@��@��@�@�/@�j@��@z�@I�@�@�
@�F@��@t�@dZ@33@
�H@
��@
�\@
~�@
n�@
n�@
=q@	�@	�#@	��@	��@	��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B	�B	�B	�B	�mB	�B	�XB	�;B	�B	��B
B
1B
JB
PB
uB
�B
�B
+B
W
B�Bm�B�bB�hB��B�^BȴB�#BɺB�NBB{B>wBE�BR�B[#BbNBffBffBu�Bp�BXB�B�B}�Bk�BF�BR�BM�BL�BM�BcTBhsBhsBe`BbNBVB=qBYBP�BP�B9XB�B�B�BPB1B��B��B�B��B��BǮB�qB�hB��B�bB�VB�1Bk�B[#BT�B:^B �BuB
��B
��B
�B
�;B
�qB
�B
��B
��B
��B
��B
��B
�%B
hsB
cTB
ZB
P�B
6FB
�B	��B	�B	�^B	��B	��B	��B	�{B	�PB	�B	m�B	VB	C�B	Q�B	Q�B	F�B	9XB	'�B	bB��B	DB	�B	VB	B�B��B	B	B	B��B��B�B�ZB�B�fB�fB�;B��B�FB�B�FB�B��B��B��B��B�oB��B��B�hB�VB}�B~�B�1B�B�B�+B�%B�Bx�Bu�BhsBz�B� B~�Bz�Bq�BhsBp�Bt�Bw�Bs�Bm�Bo�BjB]/BP�BI�BT�BQ�BS�BR�B]/BM�BP�BQ�BM�BXBaHB]/BZBdZBe`Be`Bl�BffB^5BhsBaHBYB^5Be`BaHBbNBcTB`BB_;BiyBjBjBhsBe`Be`BbNB`BB\)B\)BgmBp�Bt�Br�BgmB^5Bl�By�Bx�B�B�B�B�Bz�By�Bp�B�B�B� Bx�Bu�Bu�B{�Bz�B{�B�B}�Bs�Bz�B�+B�1B�7B�B{�B�1B�VB�oB��B��B��B��B��B��B��B��B�B�B�B�9B�!B�!B�FB�dB�qB�RB�^BŢB��B��B��BǮBȴB��B��B��B��B�
B�;B�;B�`B�sB�sB�B�B�B�B�B�B�B��B��B��B	  B	%B	+B	
=B		7B	
=B	PB	VB	�B	�B	�B	�B	�B	 �B	"�B	%�B	%�B	&�B	(�B	,B	,B	33B	5?B	9XB	:^B	;dB	:^B	;dB	B�B	C�B	D�B	F�B	H�B	I�B	K�B	M�B	O�B	O�B	Q�B	T�B	YB	\)B	`BB	bNB	dZB	gmB	gmB	gmB	l�B	o�B	p�B	p�B	v�B	y�B	z�B	z�B	z�B	z�B	|�B	~�B	� B	�B	�%B	�B	�%B	�=B	�JB	�JB	�JB	�PB	�VB	�bB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�'B	�?B	�FB	�LB	�XB	�jB	�wB	�qB	�qB	�qB	�}B	��B	B	ÖB	ƨB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�B	�B	�)B	�)B	�#B	�)B	�5B	�;B	�BB	�;B	�HB	�NB	�NB	�NB	�HB	�HB	�HB	�TB	�TB	�TB	�fB	�fB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
  B
B
B
B
B
B
B
B
%B
%B
%B
B
%B
1B
1B
1B
+B
	7B
1B

=B

=B

=B
	7B
	7B
DB
DB
DB
DB
JB
JB
JB
JB
DB
+B
DB
PB
JB
PB
PB
PB
DB
DB
JB
JB
\B
bB
hB
bB
bB
bB
oB
oB
hB
oB
�B
�B
�B
�B
�B
{B
oB
{B
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
"�B
!�B
 �B
 �B
 �B
�B
�B
!�B
#�B
$�B
$�B
%�B
%�B
%�B
%�B
%�B
&�B
&�B
$�B
&�B
'�B
&�B
'�B
)�B
(�B
'�B
'�B
)�B
)�B
+B
)�B
+B
)�B
+B
)�B
)�B
)�B
+B
)�B
.B
.B
.B
.B
/B
0!B
/B
/B
/B
0!B
0!B
1'B
1'B
1'B
2-B
2-B
2-B
2-B
1'B
2-B
2-B
49B
49B
33B
49B
49B
6FB
7LB
6FB
6FB
7LB
7LB
6FB
7LB
8RB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
8RB
6FB
6FB
:^B
;dB
;dB
;dB
:^B
9XB
:^B
;dB
<jB
;dB
;dB
;dB
:^B
:^B
;dB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
>wB
?}B
>wB
?}B
?}B
>wB
=qB
?}B
?}B
@�B
A�B
@�B
@�B
B�B
B�B
C�B
C�B
C�B
C�B
B�B
B�B
B�B
C�B
B�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
C�B
C�B
E�B
E�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
F�B
E�B
E�B
D�B
D�B
E�B
G�B
F�B
F�B
G�B
H�B
H�B
H�B
H�B
I�B
I�B
J�B
J�B
J�B
K�B
K�B
J�B
J�B
J�B
I�B
J�B
K�B
K�B
L�B
M�B
M�B
M�B
M�B
L�B
L�B
L�B
K�B
M�B
N�B
N�B
N�B
N�B
N�B
O�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
P�B
P�B
Q�B
Q�B
S�B
S�B
T�B
T�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
VB
T�B
VB
W
B
W
B
XB
W
B
VB
W
B
W
B
W
B
W
B
YB
XB
YB
ZB
[#B
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
]/B
]/B
\)B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
^5B
]/B
^5B
^5B
_;B
^5B
^5B
^5B
_;B
`BB
`BB
`BB
`BB
`BB
_;B
`BB
`BB
_;B
_;B
_;B
`BB
aHB
aHB
bNB
bNB
aHB
aHB
bNB
cTB
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
dZB
dZB
dZB
cTB
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
ffB
e`B
ffB
gmB
gmB
hsB
hsB
gmB
hsB
iyB
jB
jB
jB
iyB
iyB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
jB
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
l�B
m�B
n�B
n�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
m�B
n�B
o�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
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
s�B
s�B
r�B
t�B
t�B
t�B
s�B
t�B
t�B
t�B
t�B
s�B
s�B
t�B
u�B
u�B
t�B
t�B
t�B
u�B
u�B
u�B
v�B
v�B
u�B
u�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
v�B
w�B
w�B
w�B
x�B
x�B
w�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
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
y�B
{�B
{�B
{�B
{�B
|�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B	��B	��B	��B	�>B	ؓB	��B	�\B	�3B	�dB
;B
fB
�B
�B
�B
�B
�B
/ B
]�BKBn}B��B�&B��B�BʌB�xB�jB�B�B�B?�BF�BTB\CBc�Bg�Bg�Bv`Br-B\)B��B�B~�Bm�BK)BTaBO�BNpBN�Bc�Bh�Bh�BfLBc�BXyBA;BZ7BR�BR B<�B"�B!|BIBvB	�B�B�<B�B҉B��B�	B�iB��B��B�@B��B�XBo�B]�BWsB=B$&B9B
�B
�B
�B
��B
�;B
��B
��B
��B
��B
��B
�$B
��B
kkB
e�B
[�B
R�B
9�B
!B	��B	�;B	�.B	�3B	��B	��B	�
B	�BB	�SB	p�B	Y�B	F�B	S@B	SB	HfB	;0B	*�B	B�B	�B	KB	�B	�B�|B�]B	'B	�B	�B��B�B��B�fBۦB��B��B�BЗB��B��B�B��B��B�B��B��B�B��B�B�uB�HB�B��B��B��B�B��B��B��BzxBw2Bj�B{dB�OBHB{dBsBj0Bq�BuZBxBtTBn}Bp!BkQB^�BS@BLdBVSBS�BU�BT�B]�BPBR�BS�BP.BYKBa�B^BZ�Bd�Bf�Bf�BmCBg�B_�BiDBb�BZ�B_�BffBb�Bc�Bd�Ba�B`�Bj0BkBkBi*BfLBfLBcnBabB]�B]�BhsBq[Bu%BshBi_B`�Bm�Bz�By�B�UB��B�GB��B{�Bz�Br|B�oB�gB��By�Bv�Bv�B|�B{�B|�B�[B~�BuZB|B��B��B��B��B}�B�7B�BB�@B�B�!B�NB�ZB�ZB�mB�zB�XB�B�IB�kB��B�B�B��B��B��B�>B�JB��B��B�B�DBȀBɠBуB�FBбB��B׍B�pBߤB�zB�B��B��B��B��B�B��B�[B�9B�`B�<B�cB	 �B	tB	zB	
rB		�B	
�B	�B	�B	B	B	�B	B	B	!B	#:B	&2B	&2B	'RB	)DB	,WB	,qB	3hB	5�B	9�B	:�B	;�B	:�B	;�B	B�B	C�B	D�B	F�B	H�B	J	B	K�B	NB	O�B	P.B	R:B	U�B	YB	\]B	`vB	b�B	d�B	g�B	g�B	g�B	l�B	o�B	p�B	q'B	wB	y�B	z�B	z�B	{0B	{B	}<B	.B	�OB	�aB	�tB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	��B	�B	�B	�B	�$B	�6B	�QB	�WB	�kB	�qB	�OB	��B	�vB	��B	��B	��B	��B	��B	�wB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�"B	�BB	�4B	� B	�&B	�2B	�2B	�B	�SB	�gB	�+B	�7B	�CB	�xB	یB	�xB	ބB	ߊB	��B	�pB	�bB	�B	�hB	�B	�|B	�B	�B	�B	�B	�B	�B	�B	�yB	�B	�B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	�+B	�B	�B	�B	�*B	�B	�B	�B	�B	�B	�PB
  B
B
UB
 OB
B
mB
aB
[B
AB
3B
3B
%B
?B
YB
gB
?B
KB
KB
KB
_B
	7B
KB

=B

XB

XB
	�B
	lB
^B
xB
xB
xB
~B
~B
dB
JB
xB
�B
�B
jB
�B
�B
jB
jB
�B
�B
�B
�B
vB
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!B
!�B
#B
"B
!B
!B
 �B
 'B
 'B
!�B
#�B
%B
%B
%�B
&B
&B
&B
%�B
'B
'8B
%`B
'8B
($B
'8B
($B
*B
)*B
(>B
(>B
*KB
*0B
+B
*0B
+6B
*eB
+6B
*eB
*eB
*eB
+QB
*eB
.IB
.cB
.cB
.cB
/5B
0UB
/iB
/OB
/iB
0oB
0oB
1[B
1AB
1[B
2GB
2aB
2aB
2|B
1vB
2aB
2aB
4TB
4nB
3hB
4�B
4nB
6zB
7fB
6zB
6�B
7�B
7�B
6zB
7�B
8�B
9XB
9rB
9XB
9rB
9XB
9�B
9�B
8�B
6�B
6�B
:xB
;�B
;B
;�B
:�B
9�B
:�B
;�B
<�B
;�B
;�B
;�B
:�B
:�B
;�B
=�B
=�B
=�B
>�B
>�B
>�B
>�B
>�B
?�B
>�B
?�B
?�B
>�B
=�B
?�B
?�B
@�B
A�B
@�B
@�B
B�B
B�B
C�B
C�B
C�B
C�B
B�B
B�B
B�B
C�B
B�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
C�B
C�B
E�B
E�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
F�B
E�B
E�B
D�B
D�B
E�B
G�B
F�B
F�B
G�B
H�B
H�B
H�B
H�B
I�B
I�B
J�B
J�B
J�B
K�B
K�B
J�B
J�B
J�B
I�B
J�B
K�B
K�B
L�B
M�B
M�B
M�B
NB
L�B
MB
MB
LB
M�B
OB
N�B
N�B
OB
OB
PB
Q B
Q B
Q B
RB
Q�B
RB
R B
Q4B
QB
R:B
R:B
TB
T,B
U2B
U2B
T,B
TB
T,B
TB
UB
UB
U2B
VB
U2B
VB
W?B
W?B
XB
W$B
VSB
W$B
WYB
W?B
W?B
YKB
X_B
Y1B
Z7B
[#B
ZQB
Z7B
ZQB
Z7B
[=B
[=B
[=B
[WB
[=B
[=B
]/B
]dB
\]B
]IB
]IB
]IB
]IB
]dB
]IB
]dB
^OB
]IB
^jB
^OB
_pB
^OB
^OB
^�B
_VB
`BB
`\B
`BB
`\B
`\B
_VB
`vB
`vB
_VB
_pB
_�B
`vB
abB
abB
bhB
bNB
abB
abB
bNB
cTB
bhB
bhB
cnB
cnB
b�B
cTB
c�B
cnB
cnB
c�B
d�B
d�B
d�B
c�B
dtB
ezB
ezB
f�B
f�B
ffB
ffB
f�B
f�B
ffB
f�B
f�B
f�B
f�B
f�B
e�B
f�B
g�B
g�B
h�B
h�B
g�B
h�B
i�B
j�B
j�B
jB
iyB
iyB
h�B
h�B
h�B
h�B
h�B
i�B
i�B
j�B
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
l�B
m�B
n�B
n�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
m�B
n�B
o�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
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
s�B
s�B
r�B
t�B
t�B
t�B
s�B
t�B
t�B
t�B
t�B
s�B
s�B
t�B
u�B
u�B
t�B
t�B
t�B
u�B
u�B
u�B
v�B
v�B
u�B
u�B
v�B
w�B
w�B
xB
w�B
w�B
w�B
v�B
w�B
xB
xB
x�B
x�B
w�B
w�B
xB
w�B
w�B
xB
x�B
x�B
y�B
zB
y�B
x�B
x�B
zB
z�B
z�B
z�B
z�B
z�B
y�B
|B
{�B
|B
|B
}11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.11(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201806200039552018062000395520180620003955201806221332052018062213320520180622133205201806210035322018062100353220180621003532  JA  ARFMdecpA19c                                                                20180616093503  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180616003513  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180616003515  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180616003516  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180616003516  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180616003516  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180616003517  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180616003517  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180616003517  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180616003517                      G�O�G�O�G�O�                JA  ARUP                                                                        20180616005513                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180616153614  CV  JULD            G�O�G�O�F�U�                JM  ARCAJMQC2.0                                                                 20180619153955  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180619153955  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180620153532  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622043205  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116231515                      G�O�G�O�G�O�                