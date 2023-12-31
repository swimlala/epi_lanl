CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2017-03-23T00:35:15Z creation;2017-03-23T00:35:18Z conversion to V3.1;2019-12-19T08:12:08Z update;     
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
_FillValue                 �  ID   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M0   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  pl   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tX   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �d   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ޜ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20170323003515  20200116211515  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               eA   JA  I2_0577_101                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @��4΁� 1   @��5�� @3G$tS���d��&��I1   GPS     A   B   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Ca�fCd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^fD^� D_  D_� D`fD`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЃ3D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�3D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @x��@�z�@�z�A=qA>=qA^=qA~=qA��A��A��A��A��A��A��A��B�\B�\B�\B�\B'�\B/�\B7�\B?�\BG�\BO�\BW�\B_�\Bg�\Bo�\Bw�\B�\B�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮC��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca�=Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D x�D ��Dx�D��Dx�D��Dx�D��Dx�D��D\D��Dx�D��Dx�D��Dx�D��D	x�D	��D
x�D
��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D�\Dx�D��Dx�D��Dx�D��D x�D ��D!x�D!��D"x�D"��D#x�D#��D$x�D$��D%x�D%��D&x�D&��D'x�D'��D(x�D(��D)x�D)��D*x�D*��D+x�D+��D,x�D,��D-x�D-��D.x�D.��D/x�D/��D0x�D0��D1x�D1��D2x�D2��D3x�D3��D4x�D4��D5x�D5��D6x�D6��D7x�D7��D8x�D8��D9x�D9��D:x�D:��D;x�D;��D<x�D<��D=x�D=��D>x�D>��D?x�D?��D@x�D@��DAx�DA��DBx�DB��DCx�DC��DDx�DD��DEx�DE��DFx�DF��DGx�DG��DHx�DH��DIx�DI��DJx�DJ��DKx�DK��DLx�DL��DMx�DM��DNx�DN��DOx�DO��DPx�DP��DQx�DQ��DRx�DR��DSx�DS��DTx�DT��DUx�DU��DVx�DV��DWx�DW��DXx�DX��DYx�DY��DZx�DZ��D[x�D[��D\x�D\��D]x�D]�\D^x�D^��D_x�D_�\D`x�D`��Dax�Da��Dbx�Db��Dcx�Dc��Ddx�Dd��Dex�De��Dfx�Df��Dgx�Dg��Dhx�Dh��Dix�Di��Djx�Dj��Dkx�Dk��Dlx�Dl��Dmx�Dm��Dnx�Dn��Dox�Do��Dpx�Dp��Dqx�Dq��Drx�Dr��Dsx�Ds��Dtx�Dt��Dux�Du��Dvx�Dv��Dwx�Dw��Dxx�Dx��Dyx�Dy��Dzx�Dz��D{x�D{��D|x�D|��D}x�D}��D~x�D~��Dx�D��D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D¼{D��{D�<{D�|{Dü{D��{D�<{D�|{Dļ{D��{D�<{D�|{Dż{D��{D�<{D�|{DƼ{D��{D�<{D�|{DǼ{D��{D�<{D�|{Dȼ{D��{D�<{D�|{Dɼ{D��{D�<{D�|{Dʼ{D��{D�<{D�|{D˼{D��{D�<{D�|{D̼{D��{D�<{D�|{Dͼ{D��{D�<{D�|{Dμ{D��{D�<{D�|{Dϼ{D��{D�<{D��Dм{D��{D�<{D�|{DѼ{D��{D�<{D�|{DҼ{D��{D�<{D�|{DӼ{D��{D�<{D�|{DԼ{D��{D�<{D�|{Dռ{D��{D�<{D�|{Dּ{D��{D�<{D�|{D׼{D��{D�<{D�|{Dؼ{D���D�<{D�|{Dټ{D��{D�<{D�|{Dڼ{D��{D�<{D�|{Dۼ{D��{D�<{D�|{Dܼ{D��{D�<{D�|{Dݼ{D��{D�<{D�|{D޼{D��{D�<{D�|{D߼{D��{D�<{D�|{D�{D���D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D��D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D��{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�M�A�M�A�K�A�K�A�K�A�M�A�M�A�M�A�O�A�M�A�K�A�M�A�I�A�?}A�5?A�"�A���A���A��A��A��A��yA��TA���AΉ7A�r�A�hsA�E�A�+A�JA��A��`A͏\A�
=A�ĜA�K�A���AˑhA��TA�hsA�+A��TA�bNAȲ-A�JAǮA�+A�5?AžwAř�AŇ+A�r�A�9XA���A�33AÏ\A�C�A���A�AA�S�A���A�9XA�VA�A��TA� �A�ȴA���A�p�A�G�A��A��TA���A�M�A�ƨA�z�A�hsA�&�A��A��hA�33A�(�A��RA���A��+A�5?A�C�A���A��A���A�r�A��A�  A�C�A�7LA���A�5?A�|�A���A���A��wA�9XA���A�7LA���A�\)A�;dA��yA��A��uA��A�\)A�1A��mA��^A�l�A��RA�r�A��A��A�E�A��mA�p�A�;dA�33A���A~�9Az�AxJAw`BAwoAv-Aq��Am33Aj��Ai�Ag�
AcK�AbVA_�mA^�uA]��A\ĜA[�AZbNAXJAT��AShsAQ/AN��AK|�AG�mAD=qAC&�AB �A?t�A=��A;�TA9\)A6�A5��A5�7A41A1O�A/7LA-�A,��A+�A)7LA&�RA&r�A&VA%��A"��A!��A!\)A ��A 5?A"�A�
A;dAjA�;A�+A��A��A`BA�A�A�+A�;AXA�`A��A�-AdZA33AȴA=qA�wA�PA;dAE�A
{A	�7A�A33AM�A`BA �uA Q�A =qA 1'A jA ��A��A��A ȴA (�@��y@�=q@��@�@���@�1@��@���@���@���@��@�@�@�j@��@�r�@ꟾ@���@�/@�1@�C�@��y@�M�@�?}@�u@�1'@��m@��
@�F@���@�V@��;@�+@�E�@�p�@�7L@�&�@�%@���@۝�@�\)@�+@��H@�n�@���@�V@� �@���@��@�G�@���@�/@Η�@��/@�S�@�v�@�Ĝ@�9X@��@ǍP@�t�@���@��@�(�@�K�@���@�M�@���@��^@��/@� �@���@�|�@�l�@��@��R@�M�@�@��@��`@���@�Z@�I�@��m@���@�n�@�V@�^5@�x�@���@�  @�"�@�5?@�{@�@�X@���@�hs@�7L@��@��@�bN@�z�@���@�1'@��
@�S�@���@�=q@��T@���@��-@���@�I�@���@���@���@��^@��^@�@��#@�J@�{@�{@�7L@��/@��D@��@�S�@�5?@���@�X@��j@�bN@��@�$�@��T@��@�z�@�r�@�9X@��@��;@��P@�K�@���@���@�-@���@�p�@�hs@�G�@�%@��@���@���@�Q�@�1'@��@��m@�l�@�
=@���@��@���@���@���@�~�@�M�@��@���@���@�G�@��/@��u@� �@�  @��m@���@�
=@�v�@�-@��T@���@��h@�hs@��/@� �@��;@�ƨ@��
@���@���@��m@��@���@�S�@�o@���@�v�@��+@��\@���@��\@�ff@��@��h@�x�@�p�@�`B@�G�@���@���@�j@�Q�@�bN@��@�A�@�ƨ@���@�K�@�
=@�M�@��-@��^@���@�@���@���@��T@�ff@�n�@�^5@�J@���@���@��-@���@�`B@�/@�%@��@��u@�I�@�1'@���@�+@�v�@�J@�p�@�hs@���@��^@���@��@�`B@�V@���@�9X@��m@��w@��F@��@���@�|�@�l�@�S�@�33@��@��@�
=@���@�E�@�$�@���@��^@���@��@�x�@�/@�Ĝ@��u@�j@�Q�@�9X@�9X@�b@�dZ@��!@�$�@���@��-@�X@�%@���@��/@��j@��9@���@�z�@��u@��D@��D@�I�@��
@��@�;d@��H@�ȴ@���@���@�~�@�=q@�$�@�{@���@���@��@���@�j@�Z@��@�;d@��y@��\@�5?@���@��^@��@�X@���@��D@�9X@�P@
=@~��@~5?@}�@|j@{�
@{S�@z�!@z^5@zJ@y��@y�7@yG�@x�`@xb@w\)@v��@u�T@t��@sS�@s"�@s"�@so@r��@qx�@p��@p1'@o�@o��@o��@ol�@o�@nff@m?}@l�/@lj@lI�@l�@k�m@kdZ@k@j�!@jM�@i��@iG�@i7L@i&�@h�u@g�;@g\)@f�R@f��@fv�@fV@f$�@e��@e�@d�/@d��@d(�@c��@b�H@b~�@b^5@b-@a��@ax�@ax�@ax�@aG�@`�9@_�;@_�;@_�@_K�@^�y@^E�@]��@]`B@]?}@\�@\z�@[�F@Z�H@Z�!@Z~�@Y�#@Yhs@YX@YX@YX@Y7L@X�`@X�@XbN@X �@W�@W�P@W�@V��@V�+@V$�@U�@T�@T�@T�D@TI�@S��@SdZ@R�H@R��@R=q@R�@Q��@Q�7@P��@P�@P  @O�P@O�@N�+@M��@Mp�@MV@L��@L��@L(�@Kƨ@K��@K��@K�@K�@KdZ@K33@J�!@J-@JJ@I�#@I&�@H�`@H�u@HQ�@G�;@G�w@G|�@GK�@G+@G�@G
=@Fȴ@Fff@F$�@F@E@E`B@E/@EV@D��@D��@D��@Dj@DZ@D�@C�m@C�F@C��@C�@Ct�@CdZ@C33@B��@Bn�@A��@A�7@A7L@A�@A%@@Ĝ@@�@@bN@@ �@?�;@?��@?�P@?K�@?�@>ȴ@>v�@>E�@=�h@=?}@<��@<j@;�m@;�@;S�@;"�@:�H@:��@:n�@9��@9x�@9hs@9X@97L@97L@9�@9%@8��@8�9@8�9@8r�@81'@7;d@6ȴ@6V@6E�@6$�@5�T@5?}@4�@4�j@4��@4j@3�m@3t�@3@2�!@2n�@2^5@2�@2J@1�@1�^@1�7@1G�@0�`@0Ĝ@0�9@0�@0A�@/�;@/��@/�@/l�@/�@.�R@.ff@.@-��@-`B@-�@,�@,Z@,�@+�
@*�@*��@*��@*n�@*�@)��@)�^@)x�@)7L@(��@(�u@(A�@(  @'\)@';d@'+@&��@&�+@&E�@%�T@%@%@%@%�-@%/@$��@$I�@$(�@$(�@#�m@#�F@#t�@#"�@"�@"�\@"-@"J@!��@!�@!��@!��@!x�@!X@!%@ �9@ ��@ ��@ �@ A�@ b@�;@�@�P@\)@�@�y@ȴ@v�@{@�@��@@��@p�@`B@O�@?}@/@V@�j@j@(�@�
@��@t�@t�@dZ@33@@�@��@�\@n�@M�@-@��@x�@X@��@�9@��@�u@A�@b@�w@|�@+@�@
=@�@ȴ@�R@��@��@v�@V@$�@�T@@�h@�@p�@O�@�@V@�@z�@j@1@ƨ@ƨ@��@t�@S�@33@@�H@�\@n�@=q@-@-@�@J@��@�^@��@��@�7@%@�`@��@Ĝ@��@��@��@��@��@��@r�@bN@ �@�@��@|�@;d@+@�@�@��@5?@�@@�@`B@`B@�@�j@z�@z�@z�@z�@9X@1@�m@ƨ@�@o@
�H@
�H@
�H@
�H@
��@
�!@
^5@
=q@
=q@
-@	�#@	�^@	��@	�7111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�M�A�M�A�K�A�K�A�K�A�M�A�M�A�M�A�O�A�M�A�K�A�M�A�I�A�?}A�5?A�"�A���A���A��A��A��A��yA��TA���AΉ7A�r�A�hsA�E�A�+A�JA��A��`A͏\A�
=A�ĜA�K�A���AˑhA��TA�hsA�+A��TA�bNAȲ-A�JAǮA�+A�5?AžwAř�AŇ+A�r�A�9XA���A�33AÏ\A�C�A���A�AA�S�A���A�9XA�VA�A��TA� �A�ȴA���A�p�A�G�A��A��TA���A�M�A�ƨA�z�A�hsA�&�A��A��hA�33A�(�A��RA���A��+A�5?A�C�A���A��A���A�r�A��A�  A�C�A�7LA���A�5?A�|�A���A���A��wA�9XA���A�7LA���A�\)A�;dA��yA��A��uA��A�\)A�1A��mA��^A�l�A��RA�r�A��A��A�E�A��mA�p�A�;dA�33A���A~�9Az�AxJAw`BAwoAv-Aq��Am33Aj��Ai�Ag�
AcK�AbVA_�mA^�uA]��A\ĜA[�AZbNAXJAT��AShsAQ/AN��AK|�AG�mAD=qAC&�AB �A?t�A=��A;�TA9\)A6�A5��A5�7A41A1O�A/7LA-�A,��A+�A)7LA&�RA&r�A&VA%��A"��A!��A!\)A ��A 5?A"�A�
A;dAjA�;A�+A��A��A`BA�A�A�+A�;AXA�`A��A�-AdZA33AȴA=qA�wA�PA;dAE�A
{A	�7A�A33AM�A`BA �uA Q�A =qA 1'A jA ��A��A��A ȴA (�@��y@�=q@��@�@���@�1@��@���@���@���@��@�@�@�j@��@�r�@ꟾ@���@�/@�1@�C�@��y@�M�@�?}@�u@�1'@��m@��
@�F@���@�V@��;@�+@�E�@�p�@�7L@�&�@�%@���@۝�@�\)@�+@��H@�n�@���@�V@� �@���@��@�G�@���@�/@Η�@��/@�S�@�v�@�Ĝ@�9X@��@ǍP@�t�@���@��@�(�@�K�@���@�M�@���@��^@��/@� �@���@�|�@�l�@��@��R@�M�@�@��@��`@���@�Z@�I�@��m@���@�n�@�V@�^5@�x�@���@�  @�"�@�5?@�{@�@�X@���@�hs@�7L@��@��@�bN@�z�@���@�1'@��
@�S�@���@�=q@��T@���@��-@���@�I�@���@���@���@��^@��^@�@��#@�J@�{@�{@�7L@��/@��D@��@�S�@�5?@���@�X@��j@�bN@��@�$�@��T@��@�z�@�r�@�9X@��@��;@��P@�K�@���@���@�-@���@�p�@�hs@�G�@�%@��@���@���@�Q�@�1'@��@��m@�l�@�
=@���@��@���@���@���@�~�@�M�@��@���@���@�G�@��/@��u@� �@�  @��m@���@�
=@�v�@�-@��T@���@��h@�hs@��/@� �@��;@�ƨ@��
@���@���@��m@��@���@�S�@�o@���@�v�@��+@��\@���@��\@�ff@��@��h@�x�@�p�@�`B@�G�@���@���@�j@�Q�@�bN@��@�A�@�ƨ@���@�K�@�
=@�M�@��-@��^@���@�@���@���@��T@�ff@�n�@�^5@�J@���@���@��-@���@�`B@�/@�%@��@��u@�I�@�1'@���@�+@�v�@�J@�p�@�hs@���@��^@���@��@�`B@�V@���@�9X@��m@��w@��F@��@���@�|�@�l�@�S�@�33@��@��@�
=@���@�E�@�$�@���@��^@���@��@�x�@�/@�Ĝ@��u@�j@�Q�@�9X@�9X@�b@�dZ@��!@�$�@���@��-@�X@�%@���@��/@��j@��9@���G�O�@��u@��D@��D@�I�@��
@��@�;d@��H@�ȴ@���@���@�~�@�=q@�$�@�{@���@���@��@���@�j@�Z@��@�;d@��y@��\@�5?@���@��^@��@�X@���@��D@�9X@�P@
=@~��@~5?@}�@|j@{�
@{S�@z�!@z^5@zJ@y��@y�7@yG�@x�`@xb@w\)@v��@u�T@t��@sS�@s"�@s"�@so@r��@qx�@p��@p1'@o�@o��@o��@ol�@o�@nff@m?}@l�/@lj@lI�@l�@k�m@kdZ@k@j�!@jM�@i��@iG�@i7L@i&�@h�u@g�;@g\)@f�R@f��@fv�@fV@f$�@e��@e�@d�/@d��@d(�@c��@b�H@b~�@b^5@b-@a��@ax�@ax�@ax�@aG�@`�9@_�;@_�;@_�@_K�@^�y@^E�@]��@]`B@]?}@\�@\z�@[�F@Z�H@Z�!@Z~�@Y�#@Yhs@YX@YX@YX@Y7L@X�`@X�@XbN@X �@W�@W�P@W�@V��@V�+@V$�@U�@T�@T�@T�D@TI�@S��@SdZ@R�H@R��@R=q@R�@Q��@Q�7@P��@P�@P  @O�P@O�@N�+@M��@Mp�@MV@L��@L��@L(�@Kƨ@K��@K��@K�@K�@KdZ@K33@J�!@J-@JJ@I�#@I&�@H�`@H�u@HQ�@G�;@G�w@G|�@GK�@G+@G�@G
=@Fȴ@Fff@F$�@F@E@E`B@E/@EV@D��@D��@D��@Dj@DZ@D�@C�m@C�F@C��@C�@Ct�@CdZ@C33@B��@Bn�@A��@A�7@A7L@A�@A%@@Ĝ@@�@@bN@@ �@?�;@?��@?�P@?K�@?�@>ȴ@>v�@>E�@=�h@=?}@<��@<j@;�m@;�@;S�@;"�@:�H@:��@:n�@9��@9x�@9hs@9X@97L@97L@9�@9%@8��@8�9@8�9@8r�@81'@7;d@6ȴ@6V@6E�@6$�@5�T@5?}@4�@4�j@4��@4j@3�m@3t�@3@2�!@2n�@2^5@2�@2J@1�@1�^@1�7@1G�@0�`@0Ĝ@0�9@0�@0A�@/�;@/��@/�@/l�@/�@.�R@.ff@.@-��@-`B@-�@,�@,Z@,�@+�
@*�@*��@*��@*n�@*�@)��@)�^@)x�@)7L@(��@(�u@(A�@(  @'\)@';d@'+@&��@&�+@&E�@%�T@%@%@%@%�-@%/@$��@$I�@$(�@$(�@#�m@#�F@#t�@#"�@"�@"�\@"-@"J@!��@!�@!��@!��@!x�@!X@!%@ �9@ ��@ ��@ �@ A�@ b@�;@�@�P@\)@�@�y@ȴ@v�@{@�@��@@��@p�@`B@O�@?}@/@V@�j@j@(�@�
@��@t�@t�@dZ@33@@�@��@�\@n�@M�@-@��@x�@X@��@�9@��@�u@A�@b@�w@|�@+@�@
=@�@ȴ@�R@��@��@v�@V@$�@�T@@�h@�@p�@O�@�@V@�@z�@j@1@ƨ@ƨ@��@t�@S�@33@@�H@�\@n�@=q@-@-@�@J@��@�^@��@��@�7@%@�`@��@Ĝ@��@��@��@��@��@��@r�@bN@ �@�@��@|�@;d@+@�@�@��@5?@�@@�@`B@`B@�@�j@z�@z�@z�@z�@9X@1@�m@ƨ@�@o@
�H@
�H@
�H@
�H@
��@
�!@
^5@
=q@
=q@
-@	�#@	�^@	��@	�7111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
&�B
9XB
>wB
A�B
D�B
J�B
N�B
Q�B
XB
x�B
�B
�=B
��B
�B
ĜB
�NB
�B�BJ�B]/Bu�B�B��B�FBȴBǮBɺB�
B�B  B+B{B&�B)�B,B1'B6FB=qBA�BM�BP�BS�B^5B`BBaHBdZBl�Bs�Bu�Bv�Bw�B�B�DB�\B�hB�uB�{B��B��B��B��B��B��B��B�-B�?B�9B�FB�FB�FB�?B�?B�-B�'B�B��B�\By�Bs�Bq�BjB^5BVBD�B8RB/B#�BoB��B��B�XB�?B�-B�B��Bl�B`BBW
BT�BS�BS�BR�B?}B33B0!B�B
=B
��B
�HB
�wB
��B
�7B
|�B
\)B
C�B
:^B
7LB
/B
�B	�B	�B	��B	ĜB	��B	�oB	�+B	�B	�+B	~�B	v�B	n�B	aHB	O�B	B�B	2-B	!�B	\B��B�TB�#B�B�B�
B��B��B��BÖBBǮB��B�qB�FB�FB�-B�'B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�bB�JB�=B�JB�=B�=B�1B�1B�1B�7B�7B�PB�7B�+B�B�%B~�Br�Bu�B{�B}�B�1B�uB��B�LB�dB�^B�^B�RB�LB�qBBȴBȴBǮBǮBƨBŢBƨBŢBǮBŢBŢBƨBƨBǮBǮBȴBȴBȴBȴBɺB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�)B�/B�;B�BB�HB�TB�fB�B�B�B�sB�B�B�B�B�B��B��B��B��B	  B	B	B	%B	%B	%B	1B		7B	DB	DB	JB	\B	bB	uB	�B	�B	�B	�B	�B	�B	"�B	"�B	"�B	#�B	%�B	%�B	%�B	'�B	'�B	)�B	+B	.B	0!B	8RB	8RB	8RB	9XB	:^B	>wB	A�B	H�B	J�B	K�B	L�B	L�B	O�B	Q�B	Q�B	P�B	P�B	O�B	Q�B	Q�B	Q�B	Q�B	S�B	T�B	YB	bNB	hsB	jB	l�B	k�B	k�B	l�B	o�B	l�B	l�B	p�B	r�B	s�B	y�B	�+B	�7B	�JB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�9B	�LB	�LB	�RB	�^B	�qB	�wB	�}B	�}B	B	ÖB	ĜB	ĜB	ŢB	ǮB	ǮB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�B	�B	�B	�5B	�BB	�HB	�NB	�NB	�NB	�NB	�NB	�NB	�NB	�NB	�NB	�NB	�`B	�fB	�fB	�fB	�mB	�sB	�yB	�yB	�yB	�yB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
1B
1B
	7B

=B
DB
DB
JB
VB
hB
hB
hB
oB
oB
uB
uB
{B
uB
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
"�B
!�B
!�B
#�B
$�B
%�B
%�B
&�B
&�B
&�B
&�B
'�B
(�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
+B
+B
+B
+B
+B
,B
,B
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
1'B
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
5?B
5?B
49B
5?B
5?B
6FB
6FB
6FB
6FB
6FB
6FB
6FB
6FB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
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
=qB
=qB
=qB
>wB
>wB
>wB
>wB
>wB
?}B
?}B
?}B
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
D�B
D�B
D�B
D�B
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
F�B
G�B
G�B
G�B
F�B
G�B
G�B
G�B
H�B
H�B
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
P�B
P�B
P�B
P�B
P�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
T�B
T�B
T�B
T�B
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
XB
XB
XB
XB
XB
YB
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
^5B
^5B
_;B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
aHB
aHB
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
e`B
e`B
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
gmB
hsB
hsB
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
k�B
l�B
l�B
l�B
l�B
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
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
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
y�B
y�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
{�B
{�B
{�B
|�B
|�B
|�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
{B
{B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
'8B
9rB
>wB
A�B
D�B
J�B
N�B
RTB
X�B
x�B
�MB
��B
��B
��B
�B
�B
��B�BK^B^BvzB�9B��B�2B�RBȀB�BؓB�B �B�B9B'�B*KB,=B1vB6�B>wBCBN�BQ�BT�B^�B`�Ba�BezBm�BtBu�Bw�By>B��B��B��B��B��B��B�EB�~B��B��B�yB�KB��B��B�`B�+B�B��B��B��B�8B�B��B��B�CB��B|Bu�BtTBm�Ba�BYBF�B:�B1�B'�B$BGB�$B�DB�B�B��B�Bo�Ba�BW�BU�BT�BU�BV�BB�B5�B5B#TB�B
��B
�B
��B
�cB
�B
��B
_B
D�B
;�B
9�B
4�B
CB	�B	�	B	��B	�B	�qB	��B	��B	�3B	��B	��B	x�B	q�B	d�B	RTB	E�B	5�B	&2B	�B��B�,B�IB�eBخBٴB�B��B�;B�gB�B�B�GB��B��B�8B�?B��B��B��B��B�)B�
B�fB��B�B��B��B��B�DB�B��B��B��B��B�pB��B��B�B�FB�NB��B��B��B��B�B�B�B��B�#B��B��B�XB�B��B��B�Bs�BvFB|B~B��B�B�/B�fB�jB�JB�JB��B��B��B�GB�RB�RB�B�BǔB��B��B�B��B��B�EB��B�_B�fBȀB�7B�7B�RB�lB�XB�0B�B��B�JB͟B�B��B҉BѷBӏB�FB�B�FB�{BԯB�SB�_B�eBڠBܬB��B�B�-B��B�@B�$B��B�=B�B�yB�QB�B�!B��B�B�B��B�B��B	 �B	�B	�B	�B	�B	�B	�B		�B	xB	xB	�B	�B	�B	�B	B	�B	�B	�B	B	 \B	#�B	# B	# B	$B	&�B	&�B	&fB	(�B	(�B	*KB	+QB	.cB	0!B	8lB	8�B	8�B	9�B	:�B	>�B	A�B	IRB	K)B	L0B	M6B	MPB	P.B	R B	RTB	Q�B	QhB	P}B	R�B	R�B	R:B	R B	S�B	UB	YB	bhB	h�B	kB	l�B	k�B	lB	m)B	poB	mB	l�B	qB	r�B	shB	y�B	�zB	��B	��B	��B	��B	��B	��B	�B	�B	�@B	�2B	�mB	�KB	�/B	�OB	�UB	�nB	��B	��B	��B	��B	��B	��B	��B	��B	��B	ðB	��B	��B	��B	��B	��B	��B	�B	��B	�B	�6B	�(B	�.B	�(B	�B	� B	�4B	�NB	�TB	� B	� B	�,B	�B	�SB	�sB	ՁB	� B	�B	� B	��B	�B	��B	�?B	�$B	�_B	�EB	چB	�OB	�\B	�bB	�NB	�B	�B	�B	�B	�B	�B	�B	�hB	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	�sB	�yB	�B	�B	�B	�QB	�cB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	�	B	�>B	�*B	�B	�PB	��B	��B	�JB	�JB	��B	��B	��B	�B	�B	�"B	�PB	�6B	�BB	�HB
 B
 B
 4B
 4B
 B
;B
;B
'B
'B
'B
AB
{B
gB
3B
MB
mB
9B
9B
9B
mB
�B
mB
?B
EB
EB
fB
�B
	�B

�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�G�O�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	B
B
B
�B
�B
�B
)B
	B
	B
�B
�B
�B
�B
�B
B
B
�B
B
B
B
�B
5B
�B
B
B
�B
�B
�B
�B
�B
�B
B
!B
 'B
 B
 'B
!-B
"4B
!�B
"�B
#B
"B
"4B
$&B
%B
&B
%�B
'B
'B
'B
'RB
(XB
)*B
)DB
)B
)B
)B
)*B
*0B
*0B
*0B
+6B
+B
+6B
+B
+6B
,WB
,WB
-CB
-)B
-)B
-CB
-CB
-]B
.cB
.IB
.IB
.IB
/OB
/iB
0;B
0UB
0UB
0UB
1[B
1AB
1AB
1[B
1vB
2aB
2aB
2GB
2aB
2|B
3�B
3hB
4nB
4nB
4�B
4nB
4�B
5�B
5tB
4TB
5�B
5tB
6FB
6`B
6FB
6zB
6`B
6zB
6`B
6`B
7�B
7�B
7�B
7�B
7�B
7�B
7�B
8�B
8lB
9�B
9rB
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
=�B
=�B
=�B
>�B
>�B
>�B
>�B
>�B
?�B
?�B
?�B
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
D�B
D�B
D�B
D�B
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
F�B
G�B
G�B
G�B
F�B
G�B
G�B
G�B
H�B
H�B
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
K�B
K�B
K�B
K�B
LB
MB
MB
NB
N"B
NB
N�B
N�B
OB
N�B
N�B
OB
P.B
O�B
O�B
PB
O�B
PB
Q B
Q B
QB
Q B
Q B
Q4B
QNB
S&B
SB
SB
S&B
S&B
TFB
T,B
UB
UB
U2B
UMB
V9B
V9B
W?B
W$B
W$B
W?B
XB
XEB
XEB
XEB
XEB
XEB
YKB
Y1B
YKB
YKB
Y1B
YKB
Y1B
Z7B
ZQB
ZQB
[WB
[qB
[=B
[WB
\CB
\]B
\xB
]IB
]~B
]�B
^jB
^jB
^jB
^jB
^jB
^jB
^jB
^OB
_pB
_VB
_pB
_VB
`�B
`\B
`\B
`vB
a�B
a|B
a|B
a|B
bhB
bhB
b�B
b�B
b�B
c�B
cnB
cnB
cnB
cnB
cnB
d�B
dtB
dtB
d�B
ezB
ezB
ezB
e�B
ezB
ezB
e�B
e�B
f�B
f�B
ffB
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
hsB
h�B
h�B
iyB
i�B
iyB
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
k�B
l�B
l�B
l�B
l�B
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
uB
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
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
xB
xB
xB
x�B
x�B
y	B
x�B
y�B
y�B
y�B
y�B
y�B
zB
zB
y�B
{B
z�B
z�B
z�B
z�B
z�B
z�B
{B
|B
|B
|B
|B
|B
|B
|�B
}B
|�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.11(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201703270037172017032700371720170327003717201806221311012018062213110120180622131101201804050712052018040507120520180405071205  JA  ARFMdecpA19c                                                                20170323093508  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20170323003515  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20170323003516  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20170323003517  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20170323003517  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20170323003517  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20170323003517  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20170323003517  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20170323003517  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20170323003518                      G�O�G�O�G�O�                JA  ARUP                                                                        20170323010408                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20170323153344  CV  JULD            G�O�G�O�F�ѧ                JM  ARSQJMQC2.0                                                                 20170324000000  CF  PSAL_ADJUSTED_QCD�@ D�@ G�O�                JM  ARSQJMQC2.0                                                                 20170324000000  CF  TEMP_ADJUSTED_QCD�@ D�@ G�O�                JM  ARCAJMQC2.0                                                                 20170326153717  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20170326153717  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404221205  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622041101  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116211515                      G�O�G�O�G�O�                