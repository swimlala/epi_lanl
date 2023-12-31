CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-09-03T00:35:32Z creation;2016-09-03T00:35:34Z conversion to V3.1;2019-12-19T08:31:26Z update;     
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
resolution        =���   axis      Z        `  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     `  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \4   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  `   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ol   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  sD   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  �|   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     `  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  �L   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  ˬ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ۜ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �    HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �$   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �(   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �,   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �l   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �|   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20160903003532  20200115101519  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               !A   JA  I2_0576_033                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�������1   @���I���@;<PH��dg�����1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8ffB?��BG��BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0�C2  C4  C6  C8�C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�fC�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|y�D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՃ3D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�C3D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�3D�C3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��H@�z�@�z�A=qA>=qA^=qA~=qA��A��A��A��A��A��A��A��B�\B�\B�\B�\B'�\B/�\B7��B?(�BG(�BO�\BW�\B_�\Bg�\Bo�\Bw�\B�\B�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB���B�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮC��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/�qC1��C3��C5��C7�qC9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C�=C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��D x�D ��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��D	x�D	��D
x�D
��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��D x�D ��D!x�D!��D"x�D"��D#x�D#��D$x�D$��D%x�D%��D&x�D&��D'x�D'��D(x�D(��D)x�D)��D*x�D*��D+x�D+��D,x�D,��D-x�D-��D.x�D.��D/x�D/��D0x�D0��D1x�D1��D2x�D2��D3x�D3��D4x�D4��D5x�D5��D6x�D6��D7x�D7��D8x�D8��D9x�D9��D:x�D:��D;x�D;��D<x�D<��D=x�D=��D>x�D>��D?x�D?��D@x�D@��DAx�DA��DBx�DB��DCx�DC��DDx�DD��DEx�DE��DFx�DF��DGx�DG��DHx�DH��DIx�DI��DJx�DJ��DKx�DK��DLx�DL��DMx�DM��DNx�DN��DOx�DO��DPx�DP��DQx�DQ��DRx�DR��DSx�DS��DTx�DT��DUx�DU��DVx�DV��DWx�DW��DXx�DX��DYx�DY��DZx�DZ��D[x�D[��D\x�D\��D]x�D]��D^x�D^��D_x�D_��D`x�D`��Dax�Da��Dbx�Db��Dcx�Dc��Ddx�Dd��Dex�De��Dfx�Df��Dgx�Dg��Dhx�Dh��Dix�Di��Djx�Dj��Dkx�Dk��Dlx�Dl��Dmx�Dm��Dnx�Dn��Dox�Do��Dpx�Dp��Dqx�Dq��Drx�Dr��Dsx�Ds��Dtx�Dt��Dux�Du��Dvx�Dv��Dwx�Dw��Dxx�Dx��Dyx�Dy��Dzx�Dz��D{x�D{��D|r�D|��D}x�D}��D~x�D~��Dx�D��D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D¼{D��{D�<{D�|{Dü{D��{D�<{D�|{Dļ{D��{D�<{D�|{Dż{D��{D�<{D�|{DƼ{D��{D�<{D�|{DǼ{D��{D�<{D�|{Dȼ{D��{D�<{D�|{Dɼ{D��{D�<{D�|{Dʼ{D��{D�<{D�|{D˼{D��{D�<{D�|{D̼{D��{D�<{D�|{Dͼ{D��{D�<{D�|{Dμ{D��{D�<{D�|{Dϼ{D��{D�<{D�|{Dм{D��{D�<{D�|{DѼ{D��{D�<{D�|{DҼ{D��{D�<{D�|{DӼ{D��{D�<{D�|{DԼ{D��{D�<{D��Dռ{D��{D�<{D�|{Dּ{D��{D�<{D�|{D׼{D��{D�<{D�|{Dؼ{D��{D�<{D�|{Dټ{D��{D�<{D�|{Dڼ{D��{D�<{D�|{Dۼ{D��{D�<{D�|{Dܼ{D��{D�<{D�|{Dݼ{D��{D�<{D�|{D޼{D��{D�<{D�|{D߼{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��HD�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D��{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�?�D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D���D�?�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111AѸRAѸRAѶFAѰ!AѴ9AѴ9AѴ9AѸRAѶFAѶFAѲ-AѰ!AѶFAѶFAѧ�Aѕ�AэPA�|�A�JA�I�A΁A�33A�?}A�/A�\)A�JA�Q�A�^5A��hA�;dA�A�-A��A��+A�=qA���A�bNA��/A�  A�\)A�`BA��mA��DA�O�A�/A�E�A� �A�t�A���A���A�S�A�ZA��FA�G�A�ĜA��\A��7A���A��-A�A��TA��RA�Q�A�r�A��A��+A�
=A��A�C�A��A���A�v�A��+A��7A���A���A�\)A���A��uA�7LA�  A�x�A�VA�7LA�JA�ƨA��A�bNAA}t�Az�+AwƨAv$�At��At��Ar�HAnffAjA�Ail�AiK�Ai%Ah(�Ag�PAgXAgK�Ag/Ag�Af��AeS�Ac�hA`��A^�+A\r�A[7LAZr�AZI�AZ-AY?}AX1AW�hAV��AU��ATȴAS��AS�hASC�AR�uAQ�TAP��AO��AN��AM�wALZAK�AJz�AI�hAH  AF{AD��AC��ACXAB��ABbNABbAA��AA7LA?��A>��A>(�A=�
A="�A<n�A;��A;K�A;&�A:��A:��A9dZA7��A6jA6bA4�A3�;A333A2��A2A1oA0�A/��A/?}A.��A.�A-p�A,ffA+�A+�A)�-A)�A(�A(��A(�9A($�A'��A&��A%�hA$�+A#��A#l�A"�HA!��A!/A ��A z�A�mAbNA�A��AJA��AffA�#A�Ax�At�Ap�AhsA33A�HAA�A�HA�DAC�AA�A�A��A�uAI�A��A{AC�AJA�A�#A
��A	�A~�A�A33A&�A��A��A�jA�Ap�AI�A7LA�9A��AbNA�A Z@���@�r�@�|�@��@�S�@���@��/@�;d@�@�9X@��y@�G�@�$�@��`@�@�7L@���@�{@���@�Ĝ@�C�@��H@�=q@�@�&�@��@܋D@��;@�33@�n�@ؓu@��@���@ӶF@�~�@Ѻ^@�1@�x�@̋D@�I�@�@��@���@�l�@Ų-@Ĭ@��@�ƨ@å�@�@�&�@�bN@�+@��R@�V@���@��#@�`B@���@���@�l�@��@�V@�@���@��7@�bN@��@�X@��@�(�@�v�@�%@�r�@��;@�l�@�+@���@�ȴ@�v�@�J@�@�X@�/@�%@��/@�I�@��
@��@�~�@�=q@��T@��@���@��u@�I�@��
@�\)@�
=@��R@�v�@�@�1'@���@���@��7@��@��@��w@�+@��@���@�E�@�?}@���@�b@�"�@��@��/@�Q�@���@��j@���@�Q�@��;@��y@�^5@��#@�hs@��@�  @�+@��+@�@�@��^@���@�x�@�O�@���@��9@��D@�bN@�9X@�b@���@���@���@�$�@��@��@��D@�bN@�b@���@�ƨ@���@��P@�|�@�S�@��@�ȴ@���@�ff@�-@��@�x�@�Ĝ@��j@��@��u@��@�r�@�Q�@�(�@�  @��
@���@�ƨ@��w@�t�@�;d@�o@��@�ȴ@���@��+@�v�@�V@�5?@�{@�J@���@��-@�x�@�X@��@���@���@��@��@�\)@�
=@�+@�C�@�S�@�b@�1'@�(�@��@���@�;d@���@�ȴ@�E�@��T@���@���@�G�@�V@�bN@�1'@�b@|�@l�@~�@~V@~5?@}@|��@|��@|��@|1@z��@z�@y�^@x�@yX@x�9@x�u@xr�@xr�@x  @w;d@v��@v��@v�@vv�@vv�@vE�@v5?@v{@u�T@u�-@u��@u�@uO�@t��@t�D@s��@s��@s"�@s@r��@r�\@r^5@r=q@q��@qhs@q&�@q&�@q%@pĜ@pQ�@o�w@o�P@ol�@o;d@n�@n��@n5?@n{@m��@m`B@l��@l(�@k��@k�m@k�m@k�F@kt�@kC�@j��@j�@jJ@i�#@i��@iX@i&�@h��@hr�@hQ�@g�@g�@g��@g\)@g
=@fȴ@fff@e�@e��@e�h@e`B@d��@d��@dj@d�@c�F@cdZ@c�@c��@cS�@co@b�@b�\@a��@aG�@`��@`1'@_|�@_;d@_+@_
=@_
=@^v�@]�T@]�@]?}@]�@\9X@[�
@[��@[S�@["�@[C�@[t�@[�@["�@Z�\@ZM�@Z�@Y�^@Y�@XĜ@X�@X�@X1'@W�w@V��@V5?@U/@T��@TZ@T1@S�m@S��@S�@So@R-@Q��@Q�@Q%@P�`@P�u@P �@O��@Ol�@O+@N��@Nȴ@N�+@N{@M?}@L��@L��@L�@L�@Lz�@L1@K�
@Kƨ@K�@KS�@K"�@J��@JM�@I��@Ihs@IX@IG�@I7L@I7L@I&�@I�@H��@Hb@G�w@G�w@G|�@G\)@G\)@G;d@F�@F��@F�+@F$�@F@E�@E�T@E�T@E�T@E`B@E�@EV@D�D@DI�@D9X@D(�@C��@C��@CS�@B�H@B��@B��@Bn�@BJ@A�^@AX@A7L@A�@A�@@��@@Q�@?�;@?�w@?�@?l�@?+@?
=@>�@>��@>$�@=�h@=`B@=V@<�j@<��@<z�@<Z@<I�@<1@;��@;t�@:�H@:=q@9��@9�^@9x�@9&�@8��@8��@8�9@8��@8�@8r�@8b@7�P@7K�@7;d@7�@6�y@6��@6v�@6{@5?}@4��@4Z@41@3��@3@2��@2��@2��@1�#@1�7@1&�@0�`@0��@0�`@1%@1%@0��@0�@01'@/��@/\)@/
=@.��@.�y@.��@.V@-�T@-�T@-��@-�h@-p�@,��@,Z@+�m@+�@+t�@+t�@*��@*�\@*n�@*M�@*M�@*=q@*�@)�7@)X@)%@(�`@(�u@(Q�@(b@'�w@'|�@'l�@'+@'
=@&�y@&��@&ff@&$�@%�@%@%�h@%p�@%O�@%/@$��@$z�@$(�@#��@#�m@#�
@#�F@#��@#��@#t�@#C�@"��@"M�@!�#@!��@!�^@!x�@ ��@ r�@�;@��@\)@K�@�@
=@
=@��@��@��@�@v�@@�@`B@O�@?}@��@�m@S�@C�@�@�\@^5@M�@=q@-@-@�@�@J@��@�#@��@&�@��@�9@�u@r�@Q�@1'@ �@b@�@�w@�@�P@\)@;d@;d@
=@ȴ@�R@��@v�@ff@V@E�@E�@��@��@��@z�@z�@j@I�@(�@�@��@�m@��@"�@�H@��@��@��@�\@M�@-@�#@��@�7@x�@hs@G�@G�@G�@&�@��@�9@��@��@�@Q�@1'@�@��@�w@��@�P@�P@�P@�P@�P@|�@\)@K�@��@�R@v�@E�@��@`B@V@��@��@j@9X@1@ƨ@��@t�@C�@"�@"�@o@
�H@
�!@
n�@
=q@	�@	��@	�7@	G�@��@��@Ĝ@��@�@1'@  @�@�;@�@l�@;d@;d@;d@+@�y@ȴ@�R@��@v�@v�@ff@V@5?@@�h@p�@`B@/@V@��@�/@�/@�/@��@��@��@�j111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111AѸRAѸRAѶFAѰ!AѴ9AѴ9AѴ9AѸRAѶFAѶFAѲ-AѰ!AѶFAѶFAѧ�Aѕ�AэPA�|�A�JA�I�A΁A�33A�?}A�/A�\)A�JA�Q�A�^5A��hA�;dA�A�-A��A��+A�=qA���A�bNA��/A�  A�\)A�`BA��mA��DA�O�A�/A�E�A� �A�t�A���A���A�S�A�ZA��FA�G�A�ĜA��\A��7A���A��-A�A��TA��RA�Q�A�r�A��A��+A�
=A��A�C�A��A���A�v�A��+A��7A���A���A�\)A���A��uA�7LA�  A�x�A�VA�7LA�JA�ƨA��A�bNAA}t�Az�+AwƨAv$�At��At��Ar�HAnffAjA�Ail�AiK�Ai%Ah(�Ag�PAgXAgK�Ag/Ag�Af��AeS�Ac�hA`��A^�+A\r�A[7LAZr�AZI�AZ-AY?}AX1AW�hAV��AU��ATȴAS��AS�hASC�AR�uAQ�TAP��AO��AN��AM�wALZAK�AJz�AI�hAH  AF{AD��AC��ACXAB��ABbNABbAA��AA7LA?��A>��A>(�A=�
A="�A<n�A;��A;K�A;&�A:��A:��A9dZA7��A6jA6bA4�A3�;A333A2��A2A1oA0�A/��A/?}A.��A.�A-p�A,ffA+�A+�A)�-A)�A(�A(��A(�9A($�A'��A&��A%�hA$�+A#��A#l�A"�HA!��A!/A ��A z�A�mAbNA�A��AJA��AffA�#A�Ax�At�Ap�AhsA33A�HAA�A�HA�DAC�AA�A�A��A�uAI�A��A{AC�AJA�A�#A
��A	�A~�A�A33A&�A��A��A�jA�Ap�AI�A7LA�9A��AbNA�A Z@���@�r�@�|�@��@�S�@���@��/@�;d@�@�9X@��y@�G�@�$�@��`@�@�7L@���@�{@���@�Ĝ@�C�@��H@�=q@�@�&�@��@܋D@��;@�33@�n�@ؓu@��@���@ӶF@�~�@Ѻ^@�1@�x�@̋D@�I�@�@��@���@�l�@Ų-@Ĭ@��@�ƨ@å�@�@�&�@�bN@�+@��R@�V@���@��#@�`B@���@���@�l�@��@�V@�@���@��7@�bN@��@�X@��@�(�@�v�@�%@�r�@��;@�l�@�+@���@�ȴ@�v�@�J@�@�X@�/@�%@��/@�I�@��
@��@�~�@�=q@��T@��@���@��u@�I�@��
@�\)@�
=@��R@�v�@�@�1'@���@���@��7@��@��@��w@�+@��@���@�E�@�?}@���@�b@�"�@��@��/@�Q�@���@��j@���@�Q�@��;@��y@�^5@��#@�hs@��@�  @�+@��+@�@�@��^@���@�x�@�O�@���@��9@��D@�bN@�9X@�b@���@���@���@�$�@��@��@��D@�bN@�b@���@�ƨ@���@��P@�|�@�S�@��@�ȴ@���@�ff@�-@��@�x�@�Ĝ@��j@��@��u@��@�r�@�Q�@�(�@�  @��
@���@�ƨ@��w@�t�@�;d@�o@��@�ȴ@���@��+@�v�@�V@�5?@�{@�J@���@��-@�x�@�X@��@���@���@��@��@�\)@�
=@�+@�C�@�S�@�b@�1'@�(�@��@���@�;d@���@�ȴ@�E�@��T@���@���@�G�@�V@�bN@�1'@�b@|�@l�@~�@~V@~5?@}@|��@|��@|��@|1@z��@z�@y�^@x�@yX@x�9@x�u@xr�@xr�@x  @w;d@v��@v��@v�@vv�@vv�@vE�@v5?@v{@u�T@u�-@u��@u�@uO�@t��@t�D@s��@s��@s"�@s@r��@r�\@r^5@r=q@q��@qhs@q&�@q&�@q%@pĜ@pQ�@o�w@o�P@ol�@o;d@n�@n��@n5?@n{@m��@m`B@l��@l(�@k��@k�m@k�m@k�F@kt�@kC�@j��@j�@jJ@i�#@i��@iX@i&�@h��@hr�@hQ�@g�@g�@g��@g\)@g
=@fȴ@fff@e�@e��@e�h@e`B@d��@d��@dj@d�@c�F@cdZ@c�@c��@cS�@co@b�@b�\@a��@aG�@`��@`1'@_|�@_;d@_+@_
=@_
=@^v�@]�T@]�@]?}@]�@\9X@[�
@[��@[S�@["�@[C�@[t�@[�@["�@Z�\@ZM�@Z�@Y�^@Y�@XĜ@X�@X�@X1'@W�w@V��@V5?@U/@T��@TZ@T1@S�m@S��@S�@So@R-@Q��@Q�@Q%@P�`@P�u@P �@O��@Ol�@O+@N��@Nȴ@N�+@N{@M?}@L��@L��@L�@L�@Lz�@L1@K�
@Kƨ@K�@KS�@K"�@J��@JM�@I��@Ihs@IX@IG�@I7L@I7L@I&�@I�@H��@Hb@G�w@G�w@G|�@G\)@G\)@G;d@F�@F��@F�+@F$�@F@E�@E�T@E�T@E�T@E`B@E�@EV@D�D@DI�@D9X@D(�@C��@C��@CS�@B�H@B��@B��@Bn�@BJ@A�^@AX@A7L@A�@A�@@��@@Q�@?�;@?�w@?�@?l�@?+@?
=@>�@>��@>$�@=�h@=`B@=V@<�j@<��@<z�@<Z@<I�@<1@;��@;t�@:�H@:=q@9��@9�^@9x�@9&�@8��@8��@8�9@8��@8�@8r�@8b@7�P@7K�@7;d@7�@6�y@6��@6v�@6{@5?}@4��@4Z@41@3��@3@2��@2��@2��@1�#@1�7@1&�@0�`@0��@0�`@1%@1%@0��@0�@01'@/��@/\)@/
=@.��@.�y@.��@.V@-�T@-�T@-��@-�h@-p�@,��@,Z@+�m@+�@+t�@+t�@*��@*�\@*n�@*M�@*M�@*=q@*�@)�7@)X@)%@(�`@(�u@(Q�@(b@'�w@'|�@'l�@'+@'
=@&�y@&��@&ff@&$�@%�@%@%�h@%p�@%O�@%/@$��@$z�@$(�@#��@#�m@#�
@#�F@#��@#��@#t�@#C�@"��@"M�@!�#@!��@!�^@!x�@ ��@ r�@�;@��@\)@K�@�@
=@
=@��@��@��@�@v�@@�@`B@O�@?}@��@�m@S�@C�@�@�\@^5@M�@=q@-@-@�@�@J@��@�#@��@&�@��@�9@�u@r�@Q�@1'@ �@b@�@�w@�@�P@\)@;d@;d@
=@ȴ@�R@��@v�@ff@V@E�@E�@��@��@��@z�@z�@j@I�@(�@�@��@�m@��@"�@�H@��@��@��@�\@M�@-@�#@��@�7@x�@hs@G�@G�@G�@&�@��@�9@��@��@�@Q�@1'@�@��@�w@��@�P@�P@�P@�P@�P@|�@\)@K�@��@�R@v�@E�@��@`B@V@��@��@j@9X@1@ƨ@��@t�@C�@"�@"�@o@
�H@
�!@
n�@
=q@	�@	��@	�7@	G�@��@��@Ĝ@��@�@1'@  @�@�;@�@l�@;d@;d@;d@+@�y@ȴ@�R@��@v�@v�@ff@V@5?@@�h@p�@`B@/@V@��@�/@�/@�/@��@��@��@�j111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BQ�BQ�BQ�BP�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BP�BP�BP�BO�BN�BL�BK�BH�BE�BA�BC�BC�BJ�BC�B:^B%�B{B	7B��B�B�fB�
B��B�wB�B��B�JB�Bm�B\)BN�BE�B5?B-B"�BDB�yB��B�dB�9B�B��B��B�PB�Bt�Bs�Bq�Bo�BffB]/BT�BJ�B<jB.B(�B%�B �B�B	7BB
��B
��B
�B
�B
�`B
�NB
�B
�B
��B
��B
��B
��B
�9B
��B
��B
�B
iyB
YB
K�B
G�B
8RB

=B	�sB	�5B	�)B	�#B	��B	��B	��B	��B	��B	��B	ǮB	�}B	�9B	��B	�oB	�+B	~�B	x�B	v�B	t�B	s�B	n�B	o�B	n�B	iyB	dZB	_;B	^5B	aHB	_;B	^5B	W
B	Q�B	L�B	F�B	>wB	8RB	5?B	1'B	,B	"�B	�B	�B	�B	oB	bB	VB	PB	DB	B	B	B	PB	JB	DB		7B	+B	%B	%B	+B	B��B��B��B�B�B�B�fB�ZB�HB�)B�B�B�
B�B��B��BǮBŢB��B�qB�jB�dB�dB�XB�FB�'B��B��B��B��B��B��B�uB�oB�\B�oB�PB�+B�B~�B}�B{�Bx�Bw�Bx�Bx�Bx�Bx�Bw�Bv�Bt�Bq�Bp�Bm�Bl�BjBgmBe`BbNB`BB^5BZBVBVBP�BN�BL�BI�BE�BC�BA�BA�B@�B@�B>wB=qB<jB9XB5?B33B33B49B5?B33B2-B/B-B.B,B)�B(�B&�B$�B#�B!�B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B#�B$�B'�B)�B,B+B/B1'B1'B1'B33B5?B6FB9XB9XB:^B;dB:^B;dB=qB?}B?}B@�BA�BA�BB�BB�BE�BK�BM�BM�BN�BS�BW
BXBZB]/B^5B_;B`BBaHBcTBdZBe`BffBgmBgmBk�Bl�Bp�Bs�Bt�Bu�Bw�By�Bz�B|�B}�B� B�B�B�B�B�JB�bB�oB�oB�uB�{B��B��B��B��B��B��B��B��B�B�B�-B�LB��BĜBƨB��B��B��B��B��B��B��B��B�B�#B�)B�;B�;B�BB�BB�HB�HB�HB�HB�HB�HB�HB�HB�HB�NB�NB�`B�fB�mB�yB�yB�B�B�B�B�B�B�B��B��B��B��B��B��B	B	B	B	%B	%B	%B	+B	+B	1B		7B		7B		7B	
=B	JB	VB	hB	uB	{B	�B	�B	�B	�B	�B	�B	�B	 �B	"�B	#�B	$�B	%�B	&�B	(�B	,B	-B	/B	49B	8RB	9XB	;dB	B�B	F�B	G�B	G�B	I�B	J�B	L�B	O�B	Q�B	S�B	VB	W
B	ZB	\)B	^5B	_;B	`BB	bNB	cTB	e`B	iyB	k�B	n�B	p�B	p�B	q�B	r�B	w�B	w�B	v�B	u�B	~�B	�B	�B	�B	�%B	�%B	�B	�B	�+B	�PB	�hB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�'B	�9B	�?B	�?B	�FB	�LB	�RB	�RB	�XB	�XB	�^B	�jB	�}B	�}B	�}B	�}B	��B	��B	B	ŢB	ƨB	ƨB	ǮB	ǮB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�
B	�B	�B	�B	�#B	�/B	�5B	�5B	�;B	�;B	�;B	�BB	�BB	�HB	�TB	�ZB	�ZB	�`B	�fB	�sB	�sB	�sB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
%B
+B
+B
1B
1B
	7B
	7B

=B

=B
DB
DB
DB
PB
VB
VB
VB
VB
\B
\B
bB
bB
bB
hB
hB
oB
oB
uB
uB
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
 �B
 �B
!�B
!�B
!�B
!�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
%�B
&�B
'�B
'�B
'�B
(�B
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
-B
.B
.B
/B
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
1'B
2-B
2-B
1'B
2-B
2-B
2-B
2-B
33B
33B
33B
49B
49B
33B
49B
49B
49B
5?B
5?B
5?B
5?B
5?B
6FB
7LB
8RB
8RB
8RB
8RB
9XB
9XB
9XB
9XB
:^B
9XB
:^B
:^B
:^B
:^B
;dB
;dB
<jB
<jB
=qB
=qB
=qB
=qB
=qB
>wB
?}B
@�B
@�B
A�B
A�B
B�B
B�B
B�B
B�B
C�B
B�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
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
H�B
I�B
I�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
N�B
M�B
M�B
N�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
Q�B
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
S�B
S�B
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
XB
XB
XB
XB
XB
XB
XB
XB
XB
XB
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
]/B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
^5B
_;B
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
bNB
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
l�B
l�B
l�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BQ�BQ�BQ�BP�BQ�BQ�BRBQ�BQ�BQ�BQ�BQ�BR BRBQBQBQhBQNBQNBQ BP�BM�BJ�BH�BG�BJ�BNpBJ�BB�B+B+B�BB�lB��B�	B�BªB��B�eB�B��Bp�B^�BQ4BH1B7LB/�B'�B�B��B�B��B�tB�IB��B�VB� B�uBuZBtTBr�BqvBg�B^�BV�BL�B>BB/ B)�B'B#B�B
�B�B
��B
��B
�B
�wB
�B
�TB
ڠB
ևB
ӏB
��B
̳B
�AB
�zB
�DB
��B
�1B
k�B
Z�B
MB
J�B
="B
<B	�_B	ޞB	��B	�)B	ԕB	�.B	�"B	�B	�JB	��B	ɆB	�'B	��B	��B	��B	��B	�B	yXB	wLB	u�B	u%B	o�B	p�B	p!B	j�B	e`B	_�B	^�B	bNB	`\B	_�B	X_B	SB	N�B	HfB	?�B	9�B	6�B	3hB	.IB	$tB	�B	EB	SB	&B	�B	�B	pB	B	%B	-B	�B	"B	6B	JB		�B	�B	�B	B		B	B�wB��B�FB�B��B�kB�mB�B�hB��BںBخB�B׍B�NB�BȚB�EB�;B��B��B��B�PB�^B��B��B�DB��B�~B��B�/B�?B�,B�&B��B�,B��B�fB�3B� B}B|�ByXBxBy	Bx�By$ByrBx�BxBu�BrGBq�Bo5Bm�Bk�BhXBf2BcTBb4B_VB[qBW�BW�BR:BP}BNVBK^BF�BD3BA�BA�BABAB?�B>�B=�B:�B6B3�B4B5�B6�B4�B3�B0!B.�B/iB-)B*�B*B(
B&2B$�B#TB"�B�B�BCBB$B�B_BxB�B�B�BB�BB]B]B�BB$B�B�B �B�B BVBdB5B$�B%�B(�B+B-CB+�B/�B1[B1�B2B49B5�B72B9�B9�B:�B;�B:�B<B>B?�B@ BABA�BA�BC-BC�BG+BLdBNpBN�BP.BT�BW�BX�BZ�B]~B^jB_�B`�Ba�Bc�Bd�Be�Bf�Bg�Bh
BlBmCBqBtBu%BvFBx8BzDB{JB}qB~wB�OB�oB�uB��B��B�jB��B��B��B�B�2B�B�B�!B�'B�|B�`B��B��B��B��B��B�2B��B��B�B�)B̈́B�HB�NB�oB҉BөBյBٚBیB�xB�pB�pB�vB��B�B�|B�|B�B�|B�B�B�B��B��B�B�B�B�B�B��B��B�B��B��B��B��B�B��B�B�$B�JB�dB�cB	B	3B	SB	?B	?B	YB	_B	zB	fB		lB		lB		RB	
�B	~B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	# B	$B	%,B	&B	'8B	)�B	,qB	-CB	/iB	49B	8RB	9XB	;0B	B�B	F�B	G�B	H1B	J	B	KB	M6B	PbB	R:B	T,B	V9B	WsB	ZkB	\�B	^jB	_�B	`vB	b�B	c�B	e�B	i�B	k�B	o B	p�B	p�B	rB	sMB	xB	xB	w2B	u�B	.B	�-B	�9B	�9B	�YB	�tB	�SB	�B	�EB	��B	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�$B	�*B	�B	�B	�6B	�=B	�/B	�5B	�/B	�OB	�UB	�vB	�TB	�tB	�tB	�zB	�fB	��B	��B	�rB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	� B	�B	�&B	�@B	�2B	�B	�2B	�B	�SB	�SB	�B	�?B	�EB	�KB	�7B	�#B	�IB	�jB	�OB	ߊB	ߊB	ߊB	��B	��B	�|B	�B	�tB	�tB	�zB	�B	�B	��B	�B	�B	��B	�B	�B	�B	�B	�B	��B	��B	�B	�B	��B	��B	�B	�B	��B	�B	��B	�*B	�B	�<B	�BB	�cB
;B
UB
'B
AB
AB
-B
GB
gB
SB
tB
EB
_B
KB
�B
	lB
	lB

rB

rB
^B
^B
�B
�B
�B
VB
�B
pB
�B
�B
}B
}B
�B
�B
�B
�B
�B
�B
�B
{B
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
B
�B
�B
 �B
 �B
!�B
!�B
!�B
!�B
"�B
#B
#B
$B
$B
$B
#�B
$B
$�B
%B
%B
&2B
'8B
($B
($B
(
B
)B
*B
*0B
*0B
+6B
+QB
+6B
+QB
,WB
,"B
,=B
-)B
.IB
./B
/5B
./B
.IB
/OB
/OB
/OB
0UB
0UB
0UB
0;B
1[B
1[B
1AB
1vB
2�B
2aB
1[B
2aB
2aB
2|B
2aB
3MB
3MB
3�B
4nB
4TB
3hB
4TB
49B
49B
5?B
5tB
5tB
5ZB
5�B
6�B
7fB
8lB
8lB
8�B
8lB
9rB
9XB
9�B
9rB
:�B
9�B
:�B
:�B
:xB
:xB
;�B
;�B
<�B
<�B
=�B
=qB
=�B
=�B
=�B
>�B
?�B
@�B
@�B
A�B
A�B
B�B
B�B
B�B
B�B
C�B
B�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
G�B
H�B
H�B
H�B
H�B
IB
H�B
IB
I�B
I�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
MB
MB
N"B
M�B
M�B
NB
N"B
OB
NB
NB
OB
O�B
O�B
Q B
P�B
Q B
Q B
P�B
Q B
Q B
Q B
QB
QB
Q4B
RB
RB
S&B
SB
SB
S&B
R�B
R�B
S&B
SB
TB
TB
T,B
T,B
TB
T,B
U2B
T�B
U2B
UB
VB
VB
VB
V9B
VSB
VmB
X+B
X+B
X+B
X+B
X+B
X+B
X+B
X+B
X+B
X+B
YeB
ZQB
ZQB
Z7B
Z7B
ZQB
ZQB
Z7B
ZQB
Z7B
Z7B
ZB
[=B
[=B
[=B
[#B
\CB
\CB
\CB
\)B
\CB
\]B
\]B
]dB
]dB
]dB
^OB
^OB
^OB
^5B
^5B
^5B
_VB
^jB
_VB
_pB
_VB
_pB
`\B
`vB
`�B
a�B
abB
b�B
bhB
bhB
bhB
b�B
cnB
cnB
cnB
dtB
d�B
dtB
dtB
dtB
dtB
e�B
e�B
e�B
ezB
e�B
ezB
f�B
g�B
gmB
g�B
g�B
g�B
h�B
h�B
h�B
h�B
h�B
i�B
i�B
i�B
i�B
i�B
j�B
jB
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
l�B
l�B
l�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.11(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201609080044482016090800444820160908004448201806221213252018062212132520180622121325201804050405582018040504055820180405040558  JA  ARFMdecpA19c                                                                20160903093505  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20160903003532  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20160903003533  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20160903003533  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20160903003534  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20160903003534  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20160903003534  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20160903003534  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20160903003534  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20160903003534                      G�O�G�O�G�O�                JA  ARUP                                                                        20160903012036                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20160903153818  CV  JULD            G�O�G�O�F�?�                JM  ARCAJMQC2.0                                                                 20160907154448  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20160907154448  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404190558  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622031325  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115101519                      G�O�G�O�G�O�                