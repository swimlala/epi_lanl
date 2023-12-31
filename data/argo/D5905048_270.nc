CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-08-12T00:35:22Z creation;2018-08-12T00:35:27Z conversion to V3.1;2019-12-19T07:31:24Z update;     
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
resolution        =���   axis      Z        t  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     t  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \d   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  `D   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �`   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  �@   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     t  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  �|   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ܀   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �    HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �P   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �`   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �d   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �t   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �x   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �|   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20180812003522  20200116231517  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL              A   JA  I2_0577_270                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @�x���� 1   @�x��$�@43�PH�d`^5?|�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @9��@�  @�  A   A   AA��A`  A�  A�  A�33A�  A���A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BXffB`ffBh  Bp  Bw��B��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D}��D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�C3Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @2�\@x��@�z�@�z�A=qA?�
A^=qA~=qA��A�Q�A��A��A��A��A��A��B�\B�\B�\B�\B'�\B/�\B7�\B?�\BG�\BO�\BW��B_��Bg�\Bo�\Bw(�B(�B�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB���B�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮC��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D x�D ��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��D	x�D	��D
x�D
��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��D x�D ��D!x�D!��D"x�D"��D#x�D#��D$x�D$��D%x�D%��D&x�D&��D'x�D'��D(x�D(��D)x�D)��D*x�D*��D+x�D+��D,x�D,��D-x�D-��D.x�D.��D/x�D/��D0x�D0��D1x�D1��D2x�D2��D3x�D3��D4x�D4��D5x�D5��D6x�D6��D7x�D7��D8x�D8��D9x�D9��D:x�D:��D;x�D;��D<x�D<��D=x�D=��D>x�D>��D?x�D?��D@x�D@��DAx�DA��DBx�DB��DCx�DC��DDx�DD��DEx�DE��DFx�DF��DGx�DG��DHx�DH��DIx�DI��DJx�DJ��DKx�DK��DLx�DL��DMx�DM��DNx�DN��DOx�DO��DPx�DP��DQx�DQ��DRx�DR��DSx�DS��DTx�DT��DUx�DU��DVx�DV��DWx�DW��DXx�DX��DYx�DY��DZx�DZ��D[x�D[��D\x�D\��D]x�D]��D^x�D^��D_x�D_��D`x�D`��Dax�Da��Dbx�Db��Dcx�Dc��Ddx�Dd��Dex�De��Dfx�Df��Dgx�Dg��Dhx�Dh��Dix�Di��Djx�Dj��Dkx�Dk��Dlx�Dl��Dmx�Dm��Dnx�Dn��Dox�Do��Dpx�Dp��Dqx�Dq��Drx�Dr��Dsx�Ds��Dtx�Dt��Dux�Du��Dvx�Dv��Dwx�Dw��Dxx�Dx��Dyx�Dy��Dzx�Dz��D{x�D{��D|x�D|��D}x�D}�D~x�D~��Dx�D��D�<{D�|{D��{D��{D�<{D�|{D���D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�?�D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��HD��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D���D�<{D�|{D��{D��{D�<{D�yHD��{D��{D�<{D�|{D��{D��{D�<{D�|{D¼{D��{D�<{D�|{Dü{D��{D�<{D�|{Dļ{D��{D�<{D�|{Dż{D��{D�<{D�|{DƼ{D��{D�<{D�|{DǼ{D��{D�<{D�|{Dȼ{D��{D�?�D�|{Dɼ{D��{D�<{D�|{Dʼ{D��{D�<{D�|{D˼{D��{D�<{D�|{D̼{D��{D�<{D�|{Dͼ{D��{D�<{D�|{Dμ{D��{D�<{D�|{Dϼ{D��{D�<{D�|{Dм{D��{D�<{D�|{DѼ{D��{D�<{D�|{DҼ{D��{D�<{D�|{DӼ{D��{D�<{D�|{DԼ{D��{D�<{D�|{Dռ{D��{D�<{D�|{Dּ{D��{D�<{D�|{D׼{D��{D�<{D�|{Dؼ{D��{D�<{D�|{Dټ{D��{D�<{D�|{Dڼ{D��{D�<{D�|{Dۼ{D��{D�<{D�|{Dܼ{D��{D�<{D�|{Dݼ{D��{D�<{D�|{D޼{D��{D�<{D�|{D߼{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D��{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D���D�<{11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��#A��
A�ĜAۮAۣ�Aۡ�A۟�A۝�Aۛ�Aۙ�Aۙ�Aۙ�Aۙ�Aۙ�Aۗ�Aۗ�Aۗ�Aۙ�Aۙ�Aۗ�Aۗ�Aۙ�Aۙ�Aۙ�Aۙ�Aۙ�Aۗ�Aۉ7A�M�A؍PA���A�O�Aҏ\A�+AϺ^A�jA͇+A�JAˮA��A�I�AɮA� �A��HA��A��#A�K�A�C�A��`A�VA�hsA�oA�JA���A���A�ffA�  A��A�dZA�dZA��/A�M�A�9XA�1'A��\A��A�33A� �A��!A��uA���A��TA�|�A�p�A�VA�/A��7A�ƨA���A���A���A�^5A���A��A��jA�z�A�+A�\)A�ĜA�jA�bNA��A��TA�(�A���A��wA��A��/A���A�?}A���A�ZA��hA��PA�A��DA�VA�ƨA���A��^A�`BA��`A�p�A��A~r�Az�`Ay
=Axn�As��Aq�Ap�HAo�An��AnffAmK�AjJAf�RAd�yAd5?AbVA`$�A]+A[dZAZ�HAZ�AYoAWK�AV �AU
=AR�/AP��AN�+AL��AJ��AH�RAHAGx�AF�jAEt�ADQ�AB�!AA��AA"�A@-A>��A=33A<E�A:ȴA9`BA8�`A8��A7��A5��A5�A3�A2z�A1�wA0��A.��A+�TA*�!A)�hA(r�A&M�A$�A$bA!�A��A(�A"�AA�DA�A
=A��A�7A�DA33A�A�A��AG�A/A�`AĜAbNAhsA"�A��A�
A��A�A �A�7A/A
�yA
^5A	�Ar�A�wAC�AVA��A%A�;A��A�
A =q@�&�@�;d@���@��T@�r�@�;d@��7@�@��y@�~�@��@@�h@��@�r�@�"�@��@��@�1'@�S�@��@�V@��@�Ĝ@���@�O�@�  @�{@���@� �@�n�@���@�b@���@�M�@�-@�X@�z�@�~�@�p�@�I�@�ƨ@θR@���@́@�&�@�\)@ɲ-@��/@ț�@��;@�=q@�`B@��@�1@�ƨ@�\)@+@��-@�/@���@�Z@�1@���@�o@�~�@���@���@�7L@�z�@�|�@�o@�V@�X@�%@��`@��`@�(�@��H@�V@�$�@���@�p�@���@���@�j@�1@��w@���@�|�@�|�@�S�@�"�@�o@��@���@�E�@���@���@��@��P@�|�@�dZ@�dZ@��\@�`B@���@�z�@�z�@�Q�@��F@�S�@��@���@��R@�n�@�~�@�J@��-@��@��@�Ĝ@���@��u@�bN@�I�@� �@��
@�\)@��@���@�v�@�~�@�E�@���@��#@��^@��^@��^@���@��7@�?}@��/@��w@�|�@�S�@�K�@�;d@�o@��H@���@�=q@�@���@�G�@���@�j@��@��@���@�t�@���@�5?@��T@���@�p�@�?}@���@���@��9@�bN@� �@���@���@�t�@�C�@�@��@���@���@���@�$�@���@�`B@�/@��`@���@��j@��u@�I�@�(�@�(�@�(�@���@�l�@�+@��R@���@�v�@�$�@��T@�@��-@��h@�X@�%@��u@�z�@�I�@�1'@�b@���@�\)@�+@��y@���@�E�@�$�@��@��T@���@�X@�%@��@�j@�Q�@�1@��
@���@�\)@�"�@���@��H@���@�ȴ@��\@�ff@�=q@�{@��@��@��-@���@�Z@�9X@�  @��@�l�@�S�@�K�@�C�@�;d@��@�o@���@�ȴ@��\@�^5@�J@��#@��^@�G�@�&�@���@��/@���@�Z@�(�@�ƨ@�l�@�S�@��y@�v�@�E�@�-@�{@���@��#@��-@���@���@�x�@�`B@�G�@��@��@���@�r�@�bN@�z�@�z�@�Q�@�1'@�1'@�1@��@�|�@�dZ@�;d@��@�E�@�=q@�E�@�5?@��#@�p�@��@��@�?}@��@���@��@�I�@� �@��@~E�@}p�@}p�@}O�@|�j@{�F@{�m@{dZ@{S�@{��@{dZ@{33@{"�@{@z~�@y�7@xbN@w�@wl�@w;d@w�@v��@v��@v5?@v{@u�-@up�@u/@t��@t�@tz�@s��@s��@sS�@s@rn�@q�@q��@qX@p�`@pbN@pQ�@o�@o;d@n�@n�y@o;d@o\)@o
=@n�@o+@n5?@mp�@l��@l��@lj@l�@l1@kdZ@j^5@j=q@i��@j�@i��@i%@g�@g�;@g�w@g��@g��@h  @h  @g�@g�@g|�@gK�@g�@f�+@f�+@fV@e@e`B@d��@d�@c��@c�m@c�m@d1@c��@c�m@c�
@cƨ@cdZ@bM�@a��@a��@a��@a�^@a�^@a��@a�7@ahs@a%@`��@`�@`  @_�P@_�@^�@^��@^5?@]��@]�@]/@\�j@\z�@\I�@[ƨ@[S�@Z��@Z~�@ZM�@Y�@Y�7@Yhs@Y7L@Y7L@Y&�@X��@X�9@X�@XA�@W�@W�@W+@V��@Vȴ@VV@U��@U�@U`B@T�@T�D@TZ@T1@S�m@S�m@S��@SC�@S@R��@RM�@RJ@Q�^@Q7L@P��@P��@Pr�@O��@N��@Nv�@N5?@M��@M�@MV@L��@LI�@L9X@Kƨ@K��@K��@K��@Kt�@KdZ@KdZ@KC�@Ko@J��@Jn�@J�@I��@I7L@H��@Hr�@HA�@Hb@G��@G��@G\)@G+@G
=@Fv�@F@E�-@E��@Ep�@D�@Dz�@D(�@C�
@Ct�@C"�@B��@Bn�@B�@A��@A��@A�7@Ax�@AG�@A�@@Ĝ@@�u@@�@@1'@?�@?�P@?\)@?+@?
=@>�y@>�R@>v�@>V@>5?@=�@=��@=�@=/@<�@<��@<��@<��@<z�@<Z@<I�@<�@;�m@;ƨ@;��@;t�@;S�@;33@:�@:�!@:M�@9�@9hs@9�@8��@8A�@81'@8b@7�w@7�@7��@7;d@6�y@6�R@6ff@65?@6{@5�@5��@5?}@5�@5V@4�@4�@4j@49X@3�
@3�F@3��@3dZ@3"�@2��@2^5@1�#@1G�@0�`@0�`@0Ĝ@0��@01'@/;d@/;d@/
=@.�@.�R@.E�@-�T@-��@-�-@-`B@-/@,�@,�j@,�D@,z�@,I�@,1@+�
@+��@+�@+S�@+o@*�@*�\@*^5@*�@)�#@)�7@)7L@)�@)%@(��@( �@'�;@'�;@'�;@'�@'|�@'K�@&�@&��@&v�@&$�@%�-@%��@%�h@%`B@$��@$�@$Z@$I�@$9X@$1@#��@#�
@#�F@#��@#��@#�@#t�@#t�@#C�@#"�@"�H@"��@"�\@"M�@"-@!��@!��@!x�@!&�@ �9@ �@ bN@�@\)@K�@K�@;d@
=@�@��@5?@�@�-@�h@`B@��@�@j@9X@�m@��@t�@C�@�@��@~�@M�@��@7L@��@�u@r�@ �@�@��@�w@�P@\)@;d@��@�@��@ff@{@��@�@�@O�@V@��@�D@1@��@ƨ@��@�@S�@o@��@�\@n�@^5@-@J@�#@�^@��@��@x�@7L@�`@�u@b@�@�@�;@�;@��@�w@�w@�w@�@�P@\)@;d@+@
=@ȴ@v�@v�@v�@ff@ff@V@5?@$�@$�@{11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��#A��
A�ĜAۮAۣ�Aۡ�A۟�A۝�Aۛ�Aۙ�Aۙ�Aۙ�Aۙ�Aۙ�Aۗ�Aۗ�Aۗ�Aۙ�Aۙ�Aۗ�Aۗ�Aۙ�Aۙ�Aۙ�Aۙ�Aۙ�Aۗ�Aۉ7A�M�A؍PA���A�O�Aҏ\A�+AϺ^A�jA͇+A�JAˮA��A�I�AɮA� �A��HA��A��#A�K�A�C�A��`A�VA�hsA�oA�JA���A���A�ffA�  A��A�dZA�dZA��/A�M�A�9XA�1'A��\A��A�33A� �A��!A��uA���A��TA�|�A�p�A�VA�/A��7A�ƨA���A���A���A�^5A���A��A��jA�z�A�+A�\)A�ĜA�jA�bNA��A��TA�(�A���A��wA��A��/A���A�?}A���A�ZA��hA��PA�A��DA�VA�ƨA���A��^A�`BA��`A�p�A��A~r�Az�`Ay
=Axn�As��Aq�Ap�HAo�An��AnffAmK�AjJAf�RAd�yAd5?AbVA`$�A]+A[dZAZ�HAZ�AYoAWK�AV �AU
=AR�/AP��AN�+AL��AJ��AH�RAHAGx�AF�jAEt�ADQ�AB�!AA��AA"�A@-A>��A=33A<E�A:ȴA9`BA8�`A8��A7��A5��A5�A3�A2z�A1�wA0��A.��A+�TA*�!A)�hA(r�A&M�A$�A$bA!�A��A(�A"�AA�DA�A
=A��A�7A�DA33A�A�A��AG�A/A�`AĜAbNAhsA"�A��A�
A��A�A �A�7A/A
�yA
^5A	�Ar�A�wAC�AVA��A%A�;A��A�
A =q@�&�@�;d@���@��T@�r�@�;d@��7@�@��y@�~�@��@@�h@��@�r�@�"�@��@��@�1'@�S�@��@�V@��@�Ĝ@���@�O�@�  @�{@���@� �@�n�@���@�b@���@�M�@�-@�X@�z�@�~�@�p�@�I�@�ƨ@θR@���@́@�&�@�\)@ɲ-@��/@ț�@��;@�=q@�`B@��@�1@�ƨ@�\)@+@��-@�/@���@�Z@�1@���@�o@�~�@���@���@�7L@�z�@�|�@�o@�V@�X@�%@��`@��`@�(�@��H@�V@�$�@���@�p�@���@���@�j@�1@��w@���@�|�@�|�@�S�@�"�@�o@��@���@�E�@���@���@��@��P@�|�@�dZ@�dZ@��\@�`B@���@�z�@�z�@�Q�@��F@�S�@��@���@��R@�n�@�~�@�J@��-@��@��@�Ĝ@���@��u@�bN@�I�@� �@��
@�\)@��@���@�v�@�~�@�E�@���@��#@��^@��^@��^@���@��7@�?}@��/@��w@�|�@�S�@�K�@�;d@�o@��H@���@�=q@�@���@�G�@���@�j@��@��@���@�t�@���@�5?@��T@���@�p�@�?}@���@���@��9@�bN@� �@���@���@�t�@�C�@�@��@���@���@���@�$�@���@�`B@�/@��`@���@��j@��u@�I�@�(�@�(�@�(�@���@�l�@�+@��R@���@�v�@�$�@��T@�@��-@��h@�X@�%@��u@�z�@�I�@�1'@�b@���@�\)@�+@��y@���@�E�@�$�@��@��T@���@�X@�%@��@�j@�Q�@�1@��
@���@�\)@�"�@���@��H@���@�ȴ@��\@�ff@�=q@�{@��@��@��-@���@�Z@�9X@�  @��@�l�@�S�@�K�@�C�@�;d@��@�o@���@�ȴ@��\@�^5@�J@��#@��^@�G�@�&�@���@��/@���@�Z@�(�@�ƨ@�l�@�S�@��y@�v�@�E�@�-@�{@���@��#@��-@���@���@�x�@�`B@�G�@��@��@���@�r�@�bN@�z�@�z�@�Q�@�1'@�1'@�1@��@�|�@�dZ@�;d@��@�E�@�=q@�E�@�5?@��#@�p�@��@��@�?}@��@���@��@�I�@� �@��@~E�@}p�@}p�@}O�@|�j@{�F@{�m@{dZ@{S�@{��@{dZ@{33@{"�@{@z~�@y�7@xbN@w�@wl�@w;d@w�@v��@v��@v5?@v{@u�-@up�@u/@t��@t�@tz�@s��@s��@sS�@s@rn�@q�@q��@qX@p�`@pbN@pQ�@o�@o;d@n�@n�y@o;d@o\)@o
=@n�@o+@n5?@mp�@l��@l��@lj@l�@l1@kdZ@j^5@j=q@i��@j�@i��@i%@g�@g�;@g�w@g��@g��@h  @h  @g�@g�@g|�@gK�@g�@f�+@f�+@fV@e@e`B@d��@d�@c��@c�m@c�m@d1@c��@c�m@c�
@cƨ@cdZ@bM�@a��@a��@a��@a�^@a�^@a��@a�7@ahs@a%@`��@`�@`  @_�P@_�@^�@^��@^5?@]��@]�@]/@\�j@\z�@\I�@[ƨ@[S�@Z��@Z~�@ZM�@Y�@Y�7@Yhs@Y7L@Y7L@Y&�@X��@X�9@X�@XA�@W�@W�@W+@V��@Vȴ@VV@U��@U�@U`B@T�@T�D@TZ@T1@S�m@S�m@S��@SC�@S@R��@RM�@RJ@Q�^@Q7L@P��@P��@Pr�@O��@N��@Nv�@N5?@M��@M�@MV@L��@LI�@L9X@Kƨ@K��@K��@K��@Kt�@KdZ@KdZ@KC�@Ko@J��@Jn�@J�@I��@I7L@H��@Hr�@HA�@Hb@G��@G��@G\)@G+@G
=@Fv�@F@E�-@E��@Ep�@D�@Dz�@D(�@C�
@Ct�@C"�@B��@Bn�@B�@A��@A��@A�7@Ax�@AG�@A�@@Ĝ@@�u@@�@@1'@?�@?�P@?\)@?+@?
=@>�y@>�R@>v�@>V@>5?@=�@=��@=�@=/@<�@<��@<��@<��@<z�@<Z@<I�@<�@;�m@;ƨ@;��@;t�@;S�@;33@:�@:�!@:M�@9�@9hs@9�@8��@8A�@81'@8b@7�w@7�@7��@7;d@6�y@6�R@6ff@65?@6{@5�@5��@5?}@5�@5V@4�@4�@4j@49X@3�
@3�F@3��@3dZ@3"�@2��@2^5@1�#@1G�@0�`@0�`@0Ĝ@0��@01'@/;d@/;d@/
=@.�@.�R@.E�@-�T@-��@-�-@-`B@-/@,�@,�j@,�D@,z�@,I�@,1@+�
@+��@+�@+S�@+o@*�@*�\@*^5@*�@)�#@)�7@)7L@)�@)%@(��@( �@'�;@'�;@'�;@'�@'|�@'K�@&�@&��@&v�@&$�@%�-@%��@%�h@%`B@$��@$�@$Z@$I�@$9X@$1@#��@#�
@#�F@#��@#��@#�@#t�@#t�@#C�@#"�@"�H@"��@"�\@"M�@"-@!��@!��@!x�@!&�@ �9@ �@ bN@�@\)@K�@K�@;d@
=@�@��@5?@�@�-@�h@`B@��@�@j@9X@�m@��@t�@C�@�@��@~�@M�@��@7L@��@�u@r�@ �@�@��@�w@�P@\)@;d@��@�@��@ff@{@��@�@�@O�@V@��@�D@1@��@ƨ@��@�@S�@o@��@�\@n�@^5@-@J@�#@�^@��@��@x�@7L@�`@�u@b@�@�@�;@�;@��@�w@�w@�w@�@�P@\)@;d@+@
=@ȴ@v�@v�@v�@ff@ff@V@5?@$�@$�@{11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�jB	ƨB	�B
JB
<jB
[#B
�B
�!B
��B
��B{BI�Bq�B�B��B�uB��BBB�BB	7B�BPBPB�B&�BuB�B/B;dBI�BL�B<jBA�BH�BJ�BP�BgmB� B��B��B��B��B��B��B��B��B��B�7Bk�BI�B@�BL�B<jB�B��B�B��B�`B��B��By�BXB1'B7LB�B
�sBB
��B
�B
��B
ƨB
��B
ɺB
�wB
ɺB
�qB
��B
|�B
z�B
�7B
|�B
l�B
N�B
6FB
!�B
�B
�B	�sB	�5B	��B	�B	�NB	�B	ĜB	��B	�%B	�DB	�bB	u�B	_;B	M�B	VB	`BB	YB	D�B	0!B	.B	 �B	DB��B�B�BB�BB��B�BB�B��BBǮB�-B�jB�wB�?B��B��B��B��B��B��B��B��B�JB��B�uB�JB��B�JB� BgmB�By�Bw�BbNBjBdZBT�B>wB`BB]/BXBYB^5B`BBbNBR�BP�BP�B\)BaHBcTBcTBhsBdZBcTB\)BT�B\)B\)BK�BE�BM�BG�BZB_;B^5BYBR�BJ�BT�BYBT�B\)BS�BM�BP�BM�BE�BH�BW
BaHBaHBYB[#BXBXBe`BiyBdZBYB\)BjBl�Be`Be`Bo�Bn�Bn�Bt�Bq�Bo�Bm�BffBm�Bo�Bn�Bw�Bz�Bv�Bx�B�B�B�7B�DB�B�B|�B�B�+B�JB�=B�PB�\B�VB�B�DB��B��B��B��B��B��B��B��B��B��B��B�B�!B�!B�-B�'B�3B�FB�?B�qB�dB�RB�RB��B��B��B��B��B��BȴB��B�B�)B�)B�HB�BB�fB�mB�mB�B�B�B�B�B�B��B��B�B��B��B��B��B	B	%B	+B	
=B	DB	%B	\B	�B	�B	�B	�B	 �B	%�B	'�B	(�B	(�B	0!B	/B	2-B	49B	33B	7LB	;dB	<jB	<jB	?}B	@�B	A�B	B�B	I�B	L�B	O�B	Q�B	P�B	R�B	^5B	_;B	bNB	dZB	e`B	gmB	gmB	gmB	ffB	s�B	v�B	x�B	x�B	x�B	x�B	y�B	y�B	}�B	|�B	~�B	�B	�B	�1B	�DB	�DB	�PB	�JB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�'B	�'B	�!B	�B	�'B	�RB	�XB	�^B	�qB	�qB	�jB	�wB	�}B	B	B	B	�}B	ĜB	ÖB	ǮB	ȴB	ǮB	ɺB	��B	��B	��B	��B	��B	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	��B	��B	�
B	�B	�
B	�)B	�)B	�#B	�#B	�#B	�#B	�/B	�/B	�;B	�;B	�5B	�/B	�5B	�5B	�;B	�5B	�5B	�#B	��B	�;B	�HB	�BB	�5B	�TB	�ZB	�ZB	�`B	�`B	�ZB	�`B	�ZB	�TB	�TB	�`B	�TB	�`B	�`B	�TB	�mB	�mB	�fB	�fB	�fB	�mB	�fB	�sB	�B	�yB	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	��B	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B	��B	��B	��B
B
  B	��B	��B	��B
  B	��B	��B	��B	��B	��B
B
B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
+B
1B
1B
1B
+B
%B
1B
1B
1B
1B
1B
1B
1B
1B
1B
	7B
	7B
	7B

=B
JB
PB
PB
VB
bB
hB
hB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
�B
�B
�B
�B
�B
!�B
 �B
#�B
!�B
�B
�B
"�B
"�B
$�B
$�B
&�B
&�B
%�B
%�B
&�B
%�B
'�B
&�B
'�B
'�B
&�B
'�B
'�B
&�B
+B
,B
,B
,B
,B
,B
+B
)�B
'�B
&�B
)�B
-B
-B
-B
-B
,B
,B
+B
)�B
)�B
)�B
(�B
)�B
)�B
)�B
,B
+B
+B
,B
+B
+B
,B
-B
+B
,B
,B
.B
/B
/B
/B
1'B
1'B
2-B
1'B
0!B
1'B
1'B
1'B
0!B
1'B
1'B
2-B
33B
2-B
2-B
33B
49B
49B
49B
6FB
6FB
7LB
8RB
7LB
7LB
8RB
8RB
7LB
8RB
8RB
8RB
:^B
:^B
8RB
7LB
8RB
:^B
;dB
;dB
;dB
;dB
;dB
=qB
>wB
=qB
?}B
?}B
@�B
?}B
?}B
?}B
?}B
>wB
>wB
>wB
>wB
>wB
>wB
?}B
?}B
@�B
@�B
@�B
A�B
@�B
A�B
A�B
@�B
@�B
B�B
C�B
B�B
A�B
A�B
C�B
C�B
C�B
C�B
C�B
D�B
E�B
F�B
F�B
G�B
G�B
G�B
F�B
G�B
G�B
H�B
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
J�B
I�B
I�B
L�B
L�B
L�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
K�B
L�B
K�B
K�B
K�B
M�B
M�B
N�B
P�B
P�B
O�B
P�B
P�B
O�B
O�B
P�B
P�B
Q�B
Q�B
Q�B
P�B
Q�B
R�B
S�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
R�B
R�B
Q�B
Q�B
R�B
T�B
W
B
VB
T�B
S�B
Q�B
XB
W
B
W
B
XB
W
B
W
B
ZB
YB
YB
YB
YB
ZB
ZB
[#B
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
\)B
\)B
^5B
`BB
_;B
_;B
^5B
^5B
]/B
_;B
_;B
_;B
_;B
aHB
aHB
`BB
_;B
aHB
aHB
bNB
bNB
bNB
cTB
bNB
bNB
cTB
cTB
cTB
cTB
cTB
bNB
cTB
bNB
bNB
cTB
cTB
cTB
cTB
bNB
cTB
bNB
cTB
dZB
dZB
cTB
e`B
gmB
gmB
gmB
ffB
ffB
gmB
ffB
gmB
gmB
hsB
hsB
gmB
hsB
hsB
hsB
iyB
iyB
jB
jB
iyB
jB
jB
jB
hsB
iyB
jB
k�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
m�B
m�B
m�B
m�B
n�B
o�B
n�B
n�B
n�B
n�B
m�B
p�B
p�B
p�B
p�B
o�B
o�B
o�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
s�B
r�B
q�B
q�B
q�B
q�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
s�B
t�B
t�B
t�B
s�B
t�B
u�B
v�B
v�B
v�B
u�B
u�B
v�B
v�B
v�B
v�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	� B	� B	��B	�B
�B
?cB
^OB
��B
��B
�&B
�EBBKBr�B�[B��B��B��BÖB��B�BmBDB~B�B�B�B(XB�B�B1�B=�BKBN<B>�BC�BJXBMBS�Bi_B��B��B�B�)B�B�FB�B��B�fB��B�^Bn�BM�BCaBN�B>�B�B'B�hB�*B�B��B��B}B]dB7B:�B#TB
��B�B
�B
��B
�EB
�XB
��B
��B
�OB
�=B
�wB
�0B
��B
}B
�	B
~BB
ncB
RB
9	B
%�B
�B
/B	�wB	�-B	��B	��B	�B	�=B	ƨB	��B	�	B	�PB	��B	xRB	bNB	QhB	W�B	`�B	ZB	F�B	2�B	/�B	"�B	<B��B�GB��B�B�FB��B�	B�9B�gB�B�nB��B�}B��B�B��B�_B��B�KB��B��B�7B��B��B��B�"B��B�"B��BkB��B{�ByrBe,BlqBf2BXBB[BaHB^�BZBZ�B_pBaHBcBT�BRoBR�B]Ba�BdBc�Bh�Bd�Bc�B\�BVSB\�B\�BM�BG�BO\BI�BZ�B_�B^�BZBTBL�BVBZBVSB\�BU�BO�BRoBO�BG�BJ�BX+Ba�Ba�BZkB\CBYeBYeBe�Bi�Be,BZ�B]~Bj�BmBf�Bf�Bp;Bo5BoOBu?BraBpUBn}Bg�Bn�Bp�BpBx�B{�BxBy�B��B��B��B��B��B��B~BB��B��B��B�B��B��B��B�mB�~B�$B�B�WB��B�VB�B��B�KB�sB��B��B�}B��B��B�|B��B��B��B��B��B��B�	B�$B��B�;B�[B��B�B�BɆB˒B�mB�]BܒB�|B��B�B�B��B��B�B�B��B��B��B��B��B�B�B�>B��B�B	oB	YB	_B	
�B	�B	�B	�B	�B	�B	B	CB	!-B	&B	($B	)B	)DB	0;B	/�B	2�B	4�B	3�B	7�B	;�B	<�B	<�B	?�B	@�B	A�B	B�B	J	B	MB	O�B	R B	Q4B	S[B	^5B	_pB	bhB	dtB	e�B	g�B	g�B	g�B	g8B	s�B	v�B	x�B	x�B	y$B	y$B	zDB	z*B	~BB	}<B	HB	�[B	��B	�fB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�:B	�,B	�2B	�DB	�=B	�]B	�IB	�[B	�[B	�[B	�oB	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ªB	ªB	��B	��B	��B	��B	��B	�B	��B	�	B	��B	��B	��B	�B	�B	�	B	�B	�B	� B	�4B	�.B	�:B	�,B	�@B	�TB	�&B	�9B	�$B	�MB	�MB	�?B	�_B	�sB	�]B	�]B	�qB	�qB	�qB	�WB	�dB	�IB	�pB	�pB	�OB	�dB	�OB	�jB	�VB	�jB	�jB	یB	��B	�VB	�|B	�vB	޸B	�nB	�B	�B	�zB	�B	�B	�zB	�tB	�B	�B	�B	�B	�B	�zB	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	��B	��B	��B	��B	��B	��B	�*B	�>B	�B	��B	�*B	�$B	�2B	�B
 B	�.B	�BB	�VB
B
 4B	�BB	�6B	�<B
 B	�HB	�.B	�BB	�B	�.B
B
 B	�]B	�<B
-B
;B
B
3B
MB
MB
9B
SB
[B
�B
oB
MB
_B
fB
fB
KB
_B
tB
KB
fB
KB
fB
fB
fB
KB
�B
�B
	lB
	�B
	lB

rB
dB
�B
�B
�B
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	B
�B
B
 �B
 B
�B
�B
B
)B
!�B
 �B
#�B
"B
 B
B
"�B
"�B
$�B
$�B
&�B
&�B
%�B
&B
'B
%�B
($B
'8B
($B
(
B
'8B
($B
(>B
'8B
+B
,"B
,"B
,"B
,"B
,"B
+6B
*0B
(>B
'mB
*KB
-)B
-B
-)B
-B
,"B
,"B
+B
*KB
*B
*B
)*B
*0B
*0B
*0B
,=B
+6B
+6B
,=B
+QB
+6B
,"B
-CB
+6B
,WB
,=B
./B
/OB
/iB
/5B
1[B
1AB
2GB
1[B
0UB
1[B
1AB
1AB
0;B
1[B
1[B
2GB
3hB
2aB
2aB
3hB
4TB
4nB
4�B
6`B
6`B
7�B
8lB
7�B
7�B
8lB
8lB
7�B
8lB
8�B
8�B
:�B
:�B
8�B
7�B
8�B
:�B
;�B
;�B
;�B
;�B
;�B
=�B
>�B
=�B
?�B
?�B
@�B
?�B
?}B
?�B
?�B
>�B
>�B
>�B
>�B
>�B
>�B
?�B
?�B
@�B
@�B
@�B
A�B
@�B
A�B
A�B
@�B
@�B
B�B
C�B
B�B
A�B
A�B
C�B
C�B
C�B
C�B
C�B
D�B
E�B
F�B
F�B
G�B
G�B
G�B
F�B
G�B
G�B
H�B
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
J�B
I�B
I�B
L�B
L�B
L�B
K�B
K�B
K�B
K�B
K�B
L�B
MB
MB
MB
L�B
K�B
L�B
K�B
LB
K�B
M�B
N"B
OB
Q B
Q B
PB
Q B
Q B
PB
PB
QB
QB
RB
RB
RB
QB
RB
SB
S�B
SB
SB
SB
S&B
SB
T,B
TB
T,B
S&B
S@B
R B
R:B
S&B
UB
W
B
VB
U2B
T,B
RTB
XB
W$B
W$B
X+B
WYB
WYB
ZB
Y1B
Y1B
YKB
YKB
ZQB
ZQB
[#B
Z7B
Z7B
[WB
[WB
[WB
[=B
[=B
[WB
[=B
[WB
\CB
\CB
\]B
\CB
]dB
]IB
\xB
\]B
^OB
`\B
_VB
_pB
^jB
^jB
]dB
_pB
_pB
_pB
_pB
abB
a|B
`\B
_�B
a|B
a|B
bNB
bhB
bhB
cTB
bhB
bhB
cTB
cnB
cnB
cTB
cnB
b�B
c�B
bhB
b�B
cnB
cnB
c�B
cnB
b�B
c�B
b�B
c�B
dtB
dtB
c�B
ezB
gmB
gmB
g�B
f�B
f�B
g�B
f�B
g�B
g�B
h�B
h�B
g�B
h�B
h�B
h�B
i�B
i�B
j�B
j�B
i�B
j�B
j�B
j�B
h�B
i�B
j�B
k�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
m�B
m�B
m�B
m�B
n�B
o�B
n�B
n�B
n�B
n�B
m�B
p�B
p�B
p�B
p�B
o�B
o�B
o�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
s�B
r�B
q�B
q�B
q�B
q�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
s�B
t�B
t�B
t�B
s�B
t�B
u�B
v�B
v�B
v�B
u�B
u�B
v�B
v�B
v�B
v�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.11(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201808160035522018081600355220180816003552201808160200232018081602002320180816020023201808170027272018081700272720180817002727  JA  ARFMdecpA19c                                                                20180812093511  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180812003522  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180812003525  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180812003526  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180812003526  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180812003526  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180812003526  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180812003526  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180812003527  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180812003527                      G�O�G�O�G�O�                JA  ARUP                                                                        20180812005630                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180812153504  CV  JULD            G�O�G�O�F�ǰ                JM  ARCAJMQC2.0                                                                 20180815153552  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180815153552  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180815170023  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180816152727  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116231517                      G�O�G�O�G�O�                