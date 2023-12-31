CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-12-11T00:35:23Z creation;2016-12-11T00:35:25Z conversion to V3.1;2019-12-19T08:20:01Z update;     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20161211003523  20200116211516  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               CA   JA  I2_0577_067                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @���6� 1   @��F @3�c�A �dׇ�ݗ�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  @���A   A@  A`  A���A���A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ�C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DKfDK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @\)@�z�@�G�A=qA>=qA^=qA�
A��A��A��A��A��A��A��A��B�\B�\B�\B�\B'�\B/�\B7�\B?�\BG�\BO�\BW�\B_�\Bg�\Bo�\Bw�\B�\B�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB��{B�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮC��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY�qC[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D x�D ��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D�\Dx�D��Dx�D��D	x�D	��D
x�D
��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D�\Dx�D��Dx�D��Dr�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��D x�D ��D!x�D!��D"x�D"��D#x�D#��D$x�D$��D%x�D%��D&x�D&��D'x�D'��D(x�D(��D)x�D)��D*x�D*��D+x�D+��D,x�D,��D-x�D-��D.x�D.��D/x�D/��D0x�D0��D1x�D1��D2x�D2��D3x�D3��D4x�D4��D5x�D5��D6x�D6��D7x�D7��D8x�D8��D9x�D9��D:x�D:��D;x�D;��D<x�D<��D=x�D=��D>x�D>��D?x�D?��D@x�D@��DAx�DA��DBx�DB��DCx�DC��DDx�DD��DEx�DE��DFx�DF��DGx�DG��DHx�DH��DIx�DI��DJx�DJ�\DKx�DK��DLx�DL��DMx�DM��DNx�DN��DOx�DO��DPx�DP��DQx�DQ��DRx�DR��DSx�DS��DTx�DT��DUx�DU��DVx�DV��DWx�DW��DXx�DX��DYx�DY��DZx�DZ��D[x�D[��D\x�D\��D]x�D]��D^x�D^��D_x�D_��D`x�D`��Dax�Da��Dbx�Db��Dcx�Dc��Ddx�Dd��Dex�De��Dfx�Df��Dgx�Dg��Dhx�Dh��Dix�Di��Djx�Dj��Dkx�Dk��Dlx�Dl��Dmx�Dm��Dnx�Dn��Dox�Do��Dpx�Dp��Dqx�Dq��Drx�Dr��Dsx�Ds��Dtx�Dt��Dux�Du��Dvx�Dv��Dwx�Dw��Dxx�Dx��Dyx�Dy��Dzx�Dz��D{x�D{��D|x�D|��D}x�D}��D~x�D~��Dx�D��D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D¼{D��{D�<{D�|{Dü{D��{D�<{D�|{Dļ{D��{D�<{D�|{Dż{D��{D�<{D�|{DƼ{D��{D�<{D�|{DǼ{D��{D�<{D�|{Dȼ{D��{D�<{D�|{Dɼ{D��{D�<{D�|{Dʼ{D��{D�<{D�|{D˼{D��{D�<{D�|{D̼{D��{D�<{D�|{Dͼ{D��{D�<{D�|{Dμ{D��{D�<{D�|{Dϼ{D��{D�<{D�|{Dм{D��{D�<{D�|{DѼ{D��{D�<{D�|{DҼ{D��{D�<{D�|{DӼ{D��{D�<{D�|{DԼ{D��{D�<{D�|{Dռ{D��{D�<{D�|{Dּ{D��{D�<{D�|{D׼{D��{D�<{D�|{Dؼ{D��{D�<{D�|{Dټ{D��{D�<{D�|{Dڼ{D��{D�<{D�|{Dۼ{D��{D�<{D�|{Dܼ{D��{D�<{D�|{Dݼ{D��{D�<{D�|{D޼{D��{D�<{D�|{D߼{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D��{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A֛�AցA�|�A�|�A�z�A�z�A�z�A�x�A�|�A�|�A�~�A�~�A�~�AօAօA֧�A�ƨA��TA��Aװ!A׍PA��`A�M�A�?}AҋDA�=qA�JA��HAѼjAуA�dZA�Q�A�E�A�5?A�"�A��A��A��A�p�AэPA��A�ĜA���A˕�A�z�A�/A���A�;dA��wA�ƨA�\)A���A��HA�=qA���A���A�JA�(�A��
A�;dA���A�~�A�ĜA�XA��A�ƨA�$�A�{A�dZA�t�A���A�~�A��A��A��-A�(�A�+A�A���A���A�G�A�bA�VA��FA��A���A�=qA�I�A�VA��A�"�A�S�A��A�
=A�oA�;dA���A��wA�I�A��A�$�A�%A�/A��hA�t�A���A��DA��\A�  A~�A~��A~bA|�jAz��Ay��AxQ�Av�AtbArȴApZAlĜAj��AiK�Ah=qAg��Ag7LAf��Af��Af^5AfJAe|�Ad�uAc&�A`bNA_"�A^�`A^�uA\�jA[p�A[
=AY�
AXr�AU�mATjAR�yAP��AO�PAMx�AKhsAI?}AF�AD��AD-AB�/AA
=A?�PA>1A<ĜA;��A8�`A7l�A3��A2��A2(�A1��A0jA.n�A,ĜA*�A'��A$=qA"��A"A!�PA -A��AbNA�#A7LA�RA�A��A9XA�hA�yA9XA��AAO�A"�A  A�wA|�AE�A
�uA
-A	��A	+A�\A�HA\)A~�AA�A-AA|�A�A�9AI�A�;A j@��@���@�ƨ@��@�v�@���@�hs@�%@��u@���@�l�@�v�@���@�V@���@�@���@�G�@��@�1@�@���@��@�V@�@�j@��@�|�@���@�h@�1'@���@���@�E�@�$�@�$�@�G�@�t�@�X@���@�-@���@��#@Չ7@��@ԋD@�C�@ҸR@�=q@�E�@��T@���@���@�hs@�(�@�S�@��@���@�V@�Ĝ@�|�@ʰ!@ʗ�@�$�@ə�@�&�@�j@���@�ff@�$�@���@ÍP@��y@�E�@�G�@�Z@��
@���@�\)@�"�@�^5@���@���@��`@��@�1'@��;@��@�"�@�5?@�J@���@��@��9@�1@���@�ƨ@��w@��F@���@�l�@�dZ@���@�@�(�@���@�\)@��H@��@��y@���@��y@��@��+@��@���@��h@�p�@�V@���@��j@��@��@�9X@�b@� �@� �@�(�@�Q�@�z�@��@�r�@��u@� �@�J@��/@���@�1'@�Q�@��@�1@��@���@��@�^5@���@���@��#@�hs@�J@���@�b@���@���@�`B@�Q�@� �@��@�+@���@���@��@��@��@� �@�t�@���@���@���@��h@�&�@���@� �@�"�@�K�@��@�ff@�ff@�n�@��\@�dZ@�9X@�b@��m@��;@��;@��
@��w@��P@�\)@�S�@���@��
@�|�@�@��@�v�@�~�@��!@�n�@�$�@��T@�p�@��@�V@���@���@��`@�Ĝ@�r�@�  @��@��@�S�@��@��\@��@�V@�z�@�A�@�1@���@��m@��@��@�
=@���@�v�@�ff@�5?@��@��@�@���@���@�x�@�/@��@��u@�z�@�j@�9X@�(�@�1@�ƨ@�t�@�;d@�"�@��@�J@��-@�`B@�&�@���@���@�1'@���@��w@��w@��w@��@�t�@�;d@��@�x�@��@���@��u@�r�@� �@��@�1@��P@�
=@�v�@�V@�E�@�@�E�@�V@�ff@�n�@�n�@�=q@�{@�J@���@��@��T@��@��@���@�X@���@��@�Q�@�9X@� �@��@���@���@�@�ȴ@���@��+@�ff@�M�@�E�@�E�@�=q@���@�p�@�?}@�V@���@���@���@��D@�r�@�9X@��@�ƨ@��@���@�\)@�@��H@���@��\@�n�@�J@��h@�X@�/@�%@�Ĝ@���@��D@�1'@�(�@� �@�1@��@
=@~v�@}p�@}O�@}V@|z�@{�F@{"�@z�@z�H@z��@z-@yx�@xbN@w�w@w�P@w|�@w|�@wK�@v�y@v5?@t�@s��@sS�@r��@r^5@q��@q��@qG�@p�u@p  @o�;@o|�@n�R@nff@m�T@mO�@l�j@l1@kt�@j�!@j~�@j^5@i��@i�#@i�7@iX@i&�@hĜ@h�u@hbN@h �@g�@g�w@gl�@g�@f��@f$�@e��@d��@dZ@dZ@d9X@c�@co@b��@b�@aG�@a&�@a%@`�`@`�9@`r�@`1'@_�w@_��@_|�@_K�@_+@^�@^ff@]�@]��@]p�@]`B@]/@\�@\z�@\I�@\(�@[�F@[33@Z�H@Z��@Z��@Z-@Y��@Y�@X�9@XbN@X1'@X  @W�P@W+@V��@V�@Vv�@V5?@U�T@U�@T�@Tj@TZ@TI�@TI�@TI�@S�
@S33@R�@R~�@R=q@R-@Q��@Q��@Qx�@QX@Q7L@P �@O
=@N$�@M��@M?}@L��@LI�@L(�@L1@K�m@Kt�@J�H@J�@I��@IG�@H�`@HĜ@HA�@G�;@G�P@F��@F{@E��@E�-@E`B@D��@DI�@C�F@C33@B�!@B�@A�^@Ahs@A7L@@�u@?��@?�P@?K�@>��@>ff@>@=��@=�h@=?}@=�@<��@<�/@<��@<9X@<9X@;�
@;t�@;C�@;C�@;C�@;@:��@:^5@:=q@9��@9��@9�7@8��@8r�@8b@7�w@7|�@7�@6ff@6@5p�@5/@5�@4��@4��@4j@49X@3ƨ@3dZ@3C�@3o@3@2��@2^5@1�#@1x�@1hs@1�@0bN@0b@/�w@/�w@/�P@/K�@/�@/
=@/
=@.�y@.ȴ@.��@.v�@.ff@.V@.5?@.{@-�T@-��@-?}@-/@-/@-/@,�@+�m@+�F@+��@+"�@+@*�@*��@*M�@)��@)��@)�7@)X@)�@(��@(�u@( �@'��@'l�@'
=@&��@&�+@&5?@&@%�-@%�@$��@$��@$�D@$j@$I�@#��@#��@#�m@#��@#��@#�
@#t�@#S�@#C�@#"�@"�@"�H@"�!@"^5@"=q@!��@!�^@!��@!&�@ ��@ �`@ �u@ A�@�;@��@�P@|�@�@ff@$�@�T@��@�-@p�@�@�@��@�D@I�@�@�
@��@��@S�@�H@~�@^5@=q@�^@X@�@�@�@&�@&�@&�@�`@r�@ �@�@�w@�@|�@\)@
=@�R@V@E�@5?@$�@@�T@`B@V@�/@��@��@z�@(�@1@�m@�F@t�@dZ@dZ@dZ@dZ@dZ@o@�!@M�@��@��@�@�#@�^@X@Ĝ@Ĝ@�u@�@r�@A�@ �@�w@��@l�@��@��@�+@v�@v�@v�@ff@V@�T@��@�h@�h@�@p�@/@��@Z@I�@9X@(�@�m@�@dZ@C�@33@"�@o@
�H@
��@
M�@
J@	�@	�#@	��@	��@	�7@	x�@	G�@��@��@�u@Q�@1'@  @�@l�@l�@l�@l�@l�@\)@;d@��@5?@$�@��@�h@`B@?}@�@V@��@��@�D@9X@�m@ƨ@��@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A֛�AցA�|�A�|�A�z�A�z�A�z�A�x�A�|�A�|�A�~�A�~�A�~�AօAօA֧�A�ƨA��TA��Aװ!A׍PA��`A�M�A�?}AҋDA�=qA�JA��HAѼjAуA�dZA�Q�A�E�A�5?A�"�A��A��A��A�p�AэPA��A�ĜA���A˕�A�z�A�/A���A�;dA��wA�ƨA�\)A���A��HA�=qA���A���A�JA�(�A��
A�;dA���A�~�A�ĜA�XA��A�ƨA�$�A�{A�dZA�t�A���A�~�A��A��A��-A�(�A�+A�A���A���A�G�A�bA�VA��FA��A���A�=qA�I�A�VA��A�"�A�S�A��A�
=A�oA�;dA���A��wA�I�A��A�$�A�%A�/A��hA�t�A���A��DA��\A�  A~�A~��A~bA|�jAz��Ay��AxQ�Av�AtbArȴApZAlĜAj��AiK�Ah=qAg��Ag7LAf��Af��Af^5AfJAe|�Ad�uAc&�A`bNA_"�A^�`A^�uA\�jA[p�A[
=AY�
AXr�AU�mATjAR�yAP��AO�PAMx�AKhsAI?}AF�AD��AD-AB�/AA
=A?�PA>1A<ĜA;��A8�`A7l�A3��A2��A2(�A1��A0jA.n�A,ĜA*�A'��A$=qA"��A"A!�PA -A��AbNA�#A7LA�RA�A��A9XA�hA�yA9XA��AAO�A"�A  A�wA|�AE�A
�uA
-A	��A	+A�\A�HA\)A~�AA�A-AA|�A�A�9AI�A�;A j@��@���@�ƨ@��@�v�@���@�hs@�%@��u@���@�l�@�v�@���@�V@���@�@���@�G�@��@�1@�@���@��@�V@�@�j@��@�|�@���@�h@�1'@���@���@�E�@�$�@�$�@�G�@�t�@�X@���@�-@���@��#@Չ7@��@ԋD@�C�@ҸR@�=q@�E�@��T@���@���@�hs@�(�@�S�@��@���@�V@�Ĝ@�|�@ʰ!@ʗ�@�$�@ə�@�&�@�j@���@�ff@�$�@���@ÍP@��y@�E�@�G�@�Z@��
@���@�\)@�"�@�^5@���@���@��`@��@�1'@��;@��@�"�@�5?@�J@���@��@��9@�1@���@�ƨ@��w@��F@���@�l�@�dZ@���@�@�(�@���@�\)@��H@��@��y@���@��y@��@��+@��@���@��h@�p�@�V@���@��j@��@��@�9X@�b@� �@� �@�(�@�Q�@�z�@��@�r�@��u@� �@�J@��/@���@�1'@�Q�@��@�1@��@���@��@�^5@���@���@��#@�hs@�J@���@�b@���@���@�`B@�Q�@� �@��@�+@���@���@��@��@��@� �@�t�@���@���@���@��h@�&�@���@� �@�"�@�K�@��@�ff@�ff@�n�@��\@�dZ@�9X@�b@��m@��;@��;@��
@��w@��P@�\)@�S�@���@��
@�|�@�@��@�v�@�~�@��!@�n�@�$�@��T@�p�@��@�V@���@���@��`@�Ĝ@�r�@�  @��@��@�S�@��@��\@��@�V@�z�@�A�@�1@���@��m@��@��@�
=@���@�v�@�ff@�5?@��@��@�@���@���@�x�@�/@��@��u@�z�@�j@�9X@�(�@�1@�ƨ@�t�@�;d@�"�@��@�J@��-@�`B@�&�@���@���@�1'@���@��w@��w@��w@��@�t�@�;d@��@�x�@��@���@��u@�r�@� �@��@�1@��P@�
=@�v�@�V@�E�@�@�E�@�V@�ff@�n�@�n�@�=q@�{@�J@���@��@��T@��@��@���@�X@���@��@�Q�@�9X@� �@��@���@���@�@�ȴ@���@��+@�ff@�M�@�E�@�E�@�=q@���@�p�@�?}@�V@���@���@���@��D@�r�@�9X@��@�ƨ@��@���@�\)@�@��H@���@��\@�n�@�J@��h@�X@�/@�%@�Ĝ@���@��D@�1'@�(�@� �@�1@��@
=@~v�@}p�@}O�@}V@|z�@{�F@{"�@z�@z�H@z��@z-@yx�@xbN@w�w@w�P@w|�@w|�@wK�@v�y@v5?@t�@s��@sS�@r��@r^5@q��@q��@qG�@p�u@p  @o�;@o|�@n�R@nff@m�T@mO�@l�j@l1@kt�@j�!@j~�@j^5@i��@i�#@i�7@iX@i&�@hĜ@h�u@hbN@h �@g�@g�w@gl�@g�@f��@f$�@e��@d��@dZ@dZ@d9X@c�@co@b��@b�@aG�@a&�@a%@`�`@`�9@`r�@`1'@_�w@_��@_|�@_K�@_+@^�@^ff@]�@]��@]p�@]`B@]/@\�@\z�@\I�@\(�@[�F@[33@Z�H@Z��@Z��@Z-@Y��@Y�@X�9@XbN@X1'@X  @W�P@W+@V��@V�@Vv�@V5?@U�T@U�@T�@Tj@TZ@TI�@TI�@TI�@S�
@S33@R�@R~�@R=q@R-@Q��@Q��@Qx�@QX@Q7L@P �@O
=@N$�@M��@M?}@L��@LI�@L(�@L1@K�m@Kt�@J�H@J�@I��@IG�@H�`@HĜ@HA�@G�;@G�P@F��@F{@E��@E�-@E`B@D��@DI�@C�F@C33@B�!@B�@A�^@Ahs@A7L@@�u@?��@?�P@?K�@>��@>ff@>@=��@=�h@=?}@=�@<��@<�/@<��@<9X@<9X@;�
@;t�@;C�@;C�@;C�@;@:��@:^5@:=q@9��@9��@9�7@8��@8r�@8b@7�w@7|�@7�@6ff@6@5p�@5/@5�@4��@4��@4j@49X@3ƨ@3dZ@3C�@3o@3@2��@2^5@1�#@1x�@1hs@1�@0bN@0b@/�w@/�w@/�P@/K�@/�@/
=@/
=@.�y@.ȴ@.��@.v�@.ff@.V@.5?@.{@-�T@-��@-?}@-/@-/@-/@,�@+�m@+�F@+��@+"�@+@*�@*��@*M�@)��@)��@)�7@)X@)�@(��@(�u@( �@'��@'l�@'
=@&��@&�+@&5?@&@%�-@%�@$��@$��@$�D@$j@$I�@#��@#��@#�m@#��@#��@#�
@#t�@#S�@#C�@#"�@"�@"�H@"�!@"^5@"=q@!��@!�^@!��@!&�@ ��@ �`@ �u@ A�@�;@��@�P@|�@�@ff@$�@�T@��@�-@p�@�@�@��@�D@I�@�@�
@��@��@S�@�H@~�@^5@=q@�^@X@�@�@�@&�@&�@&�@�`@r�@ �@�@�w@�@|�@\)@
=@�R@V@E�@5?@$�@@�T@`B@V@�/@��@��@z�@(�@1@�m@�F@t�@dZ@dZ@dZ@dZ@dZ@o@�!@M�@��@��@�@�#@�^@X@Ĝ@Ĝ@�u@�@r�@A�@ �@�w@��@l�@��@��@�+@v�@v�@v�@ff@V@�T@��@�h@�h@�@p�@/@��@Z@I�@9X@(�@�m@�@dZ@C�@33@"�@o@
�H@
��@
M�@
J@	�@	�#@	��@	��@	�7@	x�@	G�@��@��@�u@Q�@1'@  @�@l�@l�@l�@l�@l�@\)@;d@��@5?@$�@��@�h@`B@?}@�@V@��@��@�D@9X@�m@ƨ@��@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
"�B
 �B
 �B
 �B
!�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
$�B
%�B
&�B
5?B
D�B
P�B
u�B
��BG�B��B%B&�BB�BM�BO�BO�BN�BN�BN�BO�BO�BN�BO�BP�BP�BS�Bv�B�7B�VB�DB�B�B�1B�PB�VB�B�^B��B�bB�B�1B�B�B�B�B�+B�%B�7B�7B�PB�=B�1B�1B�B|�By�Bp�BffB]/BQ�B� B�7B�1B�B�B{�Br�Bl�Be`BcTB]/BW
BL�BB�B6FB#�B�B��B�ZB�5B��BB�!B��B�7Bt�B_;BT�BI�B8RB�B	7B
�B
��B
��B
��B
�7B
~�B
� B
|�B
z�B
m�B
cTB
XB
K�B
<jB
1'B
 �B
	7B	��B	�B	�fB	�HB	�5B	�)B	�B	�B	��B	��B	��B	��B	�3B	��B	��B	��B	��B	�oB	�\B	�7B	�B	s�B	hsB	dZB	XB	O�B	F�B	:^B	,B	 �B	{B	\B	
=B	B��B��B��B�B�B�fB�)B�
B�B�5B�;B�B��BɺB�qB�3B�B��B��B�B��B�B�B��B��B��B��B�'B�}B�wB�wB�LB�B��B��B��B��B��B��B�oB��B��B�{B�{B�hB�bB�hB�hB�hB�hB�hB�\B�\B�\B�\B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�!B�!B�B�B�B��B�B�B�B�-B�^B�jB�wB�qB�dB�dB�dB�jB�jB�qB��BBÖBĜBÖBĜBȴB��B��B��B�B�/B�BB�NB�HB�BB�TB�mB�sB�B�B�B��B��B��B��B��B��B��B	B	%B	1B	1B		7B	JB	PB	�B	�B	�B	�B	 �B	 �B	 �B	!�B	"�B	#�B	%�B	$�B	%�B	!�B	�B	�B	"�B	"�B	&�B	,B	-B	49B	;dB	=qB	A�B	F�B	D�B	I�B	N�B	O�B	P�B	Q�B	T�B	YB	\)B	]/B	]/B	^5B	cTB	gmB	hsB	k�B	r�B	x�B	x�B	t�B	s�B	s�B	v�B	z�B	z�B	w�B	|�B	�%B	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	�uB	�hB	�\B	�bB	�hB	�uB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�XB	�jB	�qB	�wB	��B	B	ÖB	ŢB	ɺB	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�#B	�)B	�;B	�TB	�TB	�TB	�ZB	�`B	�fB	�fB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
%B
+B
+B
+B
	7B
JB
PB
VB
VB
VB
\B
\B
\B
\B
\B
\B
bB
bB
bB
bB
bB
bB
bB
bB
bB
\B
bB
bB
hB
oB
oB
uB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
"�B
"�B
"�B
#�B
$�B
$�B
%�B
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
)�B
)�B
+B
,B
,B
,B
-B
-B
-B
-B
-B
-B
-B
.B
.B
.B
.B
.B
.B
/B
/B
/B
0!B
0!B
0!B
0!B
1'B
1'B
1'B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
33B
33B
33B
33B
33B
49B
49B
49B
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
7LB
7LB
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
:^B
;dB
;dB
;dB
<jB
<jB
<jB
=qB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
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
A�B
A�B
B�B
A�B
B�B
B�B
C�B
C�B
D�B
D�B
D�B
D�B
E�B
E�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
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
P�B
P�B
Q�B
Q�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
T�B
T�B
T�B
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
W
B
XB
YB
YB
YB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
[#B
ZB
[#B
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
^5B
^5B
^5B
_;B
_;B
`BB
`BB
`BB
`BB
`BB
`BB
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
gmB
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
hsB
iyB
iyB
jB
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
l�B
l�B
l�B
l�B
l�B
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
o�B
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
q�B
r�B
r�B
r�B
r�B
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
t�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
w�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
z�B
z�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~�B
~�B
� B
� B
� B
� B
� B
� B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
"�B
 �B
 �B
 �B
!�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
$�B
%�B
&�B
5%B
DMB
P�B
u�B
�BI�B�VBzB($BC-BN<BP.BP.BOBBO(BN�BO�BO�BN�BO�BQ BP�BS�BwB��B��B��B�_B��B�xB�B��B��B�BB��B��B�B��B�B�SB��B��B�B��B��B��B�(B�JB�0B�xB��B�B}"Br�Bh>B`BTFB��B�=B��B��B��B~BBt�BnBffBeB^�BYKBN�BEB:xB'�B=B �B��B��BӏB�B��B��B��Bw�B`�BW$BL�B<PBCBB
��B
רB
��B
�B
��B
�B
�B
~�B
|�B
oOB
e`B
Z�B
N�B
>�B
4�B
$�B
�B	�xB	��B	�B	��B	ޞB	�xB	��B	خB	��B	�TB	��B	�gB	��B	��B	��B	��B	�!B	�[B	�B	�xB	��B	u�B	j�B	gB	ZB	R�B	IlB	=VB	/ B	#B	�B	NB	�B	EB	 �B��B��B�2B�!B�eB�~B�B�WB�'B��BۦB�2B�jB��B�B�"B��B��B��B��B��B��B��B��B��B��B�GB��B��B��B��B��B�tB�)B�eB�B�vB��B�@B�mB��B��B��B�[B�hB��B��B��B�TB� B��B�.B�}B��B�9B�B�_B�+B�B�B�	B�	B�#B�)B�CB�dB�dB�B�!B��B�vB�pB�;B�pB��B�B��B��B�~B�dB�B�]B�vB��B��B��B��B��B�6B�oB�B�oB��B�wB�B�6B�]B��B��B��B�VB��B��B��B��B��B��B�B�BB�B��B��B�9B��BŢB�7B��B�JB�}B֡B�B�HB�B�B�-B�ZB�
B�*B�6B�5B�-B�B�B�+B�lB�JB�PB�wB	aB	tB	�B	�B		�B	�B	�B	�B	�B	IB	;B	 �B	 �B	 �B	!�B	"�B	$&B	&2B	%`B	&�B	"�B	/B	 B	#:B	"�B	'B	,"B	,�B	49B	;�B	=�B	A�B	G_B	D�B	J	B	OB	PB	Q B	R:B	U2B	YKB	\)B	]IB	]/B	^OB	cTB	g�B	h�B	k�B	shB	z*B	y�B	u%B	tB	s�B	v�B	{B	{�B	xB	|�B	�B	�NB	��B	�B	��B	�mB	�;B	��B	��B	�yB	��B	� B	��B	��B	��B	��B	�oB	�[B	�aB	�SB	�B	�5B	�BB	��B	�:B	�hB	�B	�-B	�BB	�VB	��B	�-B	�:B	��B	��B	��B	��B	��B	�rB	��B	��B	��B	��B	ªB	��B	��B	ɺB	��B	��B	�FB	�FB	�,B	�,B	�B	�$B	�kB	�qB	ܒB	ߊB	�B	�B	�B	�tB	�B	�B	�B	��B	�B	��B	��B	��B	�)B	�5B	�B	� B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	�B	�B	�	B	��B	��B	��B	�$B	�DB	�JB	�"B	�"B	�"B	�"B	�"B	�B	�(B	�.B	�HB	�HB	�cB
 �B	�]B	�<B	�6B	�B	�JB	�6B	�B	�B	�B	�B
 B
 4B
 OB	�}B	��B	�PB	�B	��B	�	B	�$B	�	B	�	B	�>B	�dB	�PB	�B	�B	�"B	��B
'B
-B
B
3B
MB
MB
%B
_B
EB
+B
	RB
dB
�B
�B
�B
�B
�B
vB
vB
�B
�B
�B
�B
�B
�B
�B
�B
�B
}B
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
B
�B
�B
B
!B
B
�B
�B
�B
 B
!-B
"NB
#B
"�B
"�B
#B
"�B
#B
#:B
$@B
%,B
%,B
&2B
&2B
&2B
&B
&2B
&2B
'8B
'B
'8B
(>B
(>B
)DB
)DB
)_B
*eB
*0B
+QB
,=B
,=B
,"B
-)B
-)B
-)B
-)B
-)B
-CB
-CB
./B
./B
./B
./B
.cB
.cB
/OB
/iB
/iB
0UB
0UB
0;B
0�B
1vB
1[B
1�B
2aB
2GB
2aB
2aB
2aB
2aB
2aB
3MB
3hB
3hB
3hB
3hB
4nB
4nB
4�B
4nB
5ZB
5ZB
5tB
5tB
5�B
6`B
6`B
6�B
6�B
7�B
7fB
7fB
7�B
8�B
8�B
8�B
9�B
9rB
9rB
9�B
:xB
:�B
:�B
:�B
;�B
;�B
;�B
<�B
<�B
<�B
=qB
=�B
=�B
=�B
=�B
>�B
>�B
>�B
>�B
>�B
>�B
>�B
>�B
>�B
>�B
@ B
?�B
@�B
@�B
A�B
A�B
B�B
A�B
B�B
B�B
C�B
C�B
D�B
D�B
D�B
D�B
E�B
E�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
H�B
H�B
IB
IB
IB
I�B
I�B
I�B
J#B
J�B
J�B
J�B
K�B
LB
K�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
MB
M�B
M�B
M�B
M�B
NB
N�B
OB
N�B
O(B
N�B
OB
O(B
PB
PB
PB
O�B
Q4B
QNB
R:B
R:B
SB
S&B
SB
S&B
TB
T,B
TFB
UB
U2B
U2B
V9B
VB
V9B
V9B
W?B
W?B
W?B
WYB
XEB
YKB
YB
Y1B
ZQB
ZQB
Z7B
ZB
ZQB
Z7B
Z7B
ZQB
Z7B
ZB
ZQB
[WB
Z7B
[WB
\CB
\CB
\)B
\]B
\xB
\]B
]dB
]dB
]IB
]IB
^OB
^jB
^OB
_VB
_pB
`vB
`\B
`\B
`\B
`vB
`�B
a|B
abB
b�B
bhB
bhB
b�B
bhB
bhB
b�B
b�B
cnB
cnB
c�B
cnB
cnB
cnB
cnB
dtB
dtB
d�B
d�B
ezB
e`B
e�B
ezB
ezB
e�B
f�B
f�B
f�B
f�B
g�B
g�B
g�B
g�B
g�B
h�B
h�B
h�B
h�B
h�B
h�B
h�B
i�B
i�B
j�B
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
l�B
l�B
l�B
l�B
l�B
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
o�B
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
q�B
r�B
r�B
r�B
r�B
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
t�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
w�B
v�B
v�B
xB
xB
w�B
w�B
xB
w�B
xB
w�B
x�B
x�B
x�B
y	B
y$B
y�B
y�B
y�B
y�B
y�B
zB
{B
{B
{�B
|B
|B
|B
|B
|B
|B
}B
|�B
}"B
}"B
}"B
}B
}"B
~B
~B
~B
~B
~(B
~(B
~(B
~(B
~B
~(B
~B
~(B
B
B
�4B
� B
� B
� B
� B
�B
�4B
�4B
�B
� B
�;B
� B
� B
� B
�'B
�'B
�'B
�AB
�'B
�AB
�-B
�3B
�3B
�B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.11(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201612141042422016121410424220161214104242201806221306142018062213061420180622130614201804050706282018040507062820180405070628  JA  ARFMdecpA19c                                                                20161211093506  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20161211003523  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20161211003524  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20161211003524  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20161211003525  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20161211003525  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20161211003525  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20161211003525  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20161211003525  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20161211003525                      G�O�G�O�G�O�                JA  ARUP                                                                        20161211013248                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20161211153239  CV  JULD            G�O�G�O�F��                JM  ARCAJMQC2.0                                                                 20161214014242  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20161214014242  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404220628  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622040614  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116211516                      G�O�G�O�G�O�                