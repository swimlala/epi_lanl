CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:06:40Z AOML 3.0 creation; 2016-05-31T21:48:58Z UW 3.1 conversion     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7    PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7X   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U|   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _p   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gd   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  id   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qX   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yL   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {L   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �@   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �4   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �d   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �d   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �d   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �d   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20140721230640  20160531144858  5903738 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               
A   AO  4053_7107_010                   2C  D   APEX                            5370                            041511                          846 @�CՉ{` 1   @�Cָ�@8�I�^�b���S��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    
A   A   A   @@  @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ�C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dtl�Dy� D��D�0 D���D�ɚD���D�@ D���D��3D�3D�0 D���D���D��D�FfDڜ�D��D�	�D�6fD�vf11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @8��@x��@�z�@�z�A=qA>=qA^=qA~=qA��A��A��A��A��A��A��A��B�\B�\B�\B�\B'�\B/�\B7�\B?�\BG�\BO�\BW�\B_�\Bg�\Bo�\Bw�\B�\B�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮC��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY�qC[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D x�D ��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��D	x�D	��D
x�D
��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��D x�D ��D!x�D!��D"x�D"��D#x�D#��D$x�D$��D%x�D%��D&x�D&��D'x�D'��D(x�D(��D)x�D)��D*x�D*��D+x�D+��D,x�D,��D-x�D-��D.x�D.��D/x�D/��D0x�D0��D1x�D1��D2x�D2��D3x�D3��D4x�D4��D5x�D5��D6x�D6��D7x�D7��D8x�D8��D9x�D9��D:x�D:��D;x�D;��D<x�D<��D=x�D=��D>x�D>��D?x�D?��D@x�D@��DAx�DA��DBx�DB��DCx�DC��DDx�DD��DEx�DE��DFx�DF��DGx�DG��DHx�DH��DIx�DI��DJx�DJ��DKx�DK��DLx�DL��DMx�DM��DNx�DN��DOx�DO��DPx�DP��DQx�DQ��DRx�DR��DSx�DS��DTx�DT��DUx�DU��DVx�DV��DWx�DW��DXx�DX��DYx�DY��DZx�DZ��D[x�D[��D\x�D\��D]x�D]��D^x�D^��D_x�D_��D`x�D`��Dax�Da��Dbx�Db��Dcx�Dc��Ddx�Dd��Dex�De��Dfx�Df��Dgx�Dg��Dhx�Dh��Dix�Di��Djx�Dj��Dkx�Dk��Dlx�Dl��Dmx�Dm��Dnx�Dn��Dox�Do��Dpx�Dp��Dqx�Dq��Drx�Dr��Dsx�Ds��Dte�Dy��D�	HD�,{D��HD��D��HD�<{D��HD���D���D�,{D��D��HD�	HD�B�DڙHD�HD�D�2�D�r�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�VA�VA�VA�Q�A�I�A�n�A��`A��A���A��wA��jA��-A��!A��A���A���A���A���A���A��PA�%A��A��9A�?}A�ƨA�VA��A�(�A�  A���A��A�x�A��A�bNA��A���A���A�\)A�?}A�33A�&�A�A���A�n�A�C�A�33A�33A��A�hsA��A���A��yA��!A��!A��A�VA���A��jA��DA��A�dZA��A�I�A��A���A�|�A�p�A�^5A�=qA�%A��jA�jA�$�A��A�Q�A�ffA�A��HA�bNA���A�1A�33A�A�I�A�E�A��yA�9XA���A��jA���A�dZA�;dA�=qA�Q�A��uA�ffA�^5A�5?A���A�oA�~�A���A�VA��A��hA��AA~�uA{Az{Av�RAv1At�/Ar�RAo�^An�DAm`BAl(�Ai`BAh(�Ag��Ag&�Afn�Ac;dAa�A_��A\�A["�AW�^AVAUhsAT�ATM�ASC�ARjAQ�
AQ33AP1'AM
=AJ~�AI�
AH��AFbNAE��AD��AD=qAC��AC�FAC��AC�mAD�AC��AC��AC&�AB�\AA��AAx�A@ffA<�A:�A:$�A9ƨA8�A7oA6(�A5�mA4Q�A2�!A1�-A0z�A/��A.�9A.Q�A-�A-O�A,��A,�!A,��A,�\A,=qA*��A)�hA(v�A'�-A& �A$��A#�A#33A"��A"�A!��A �A�mA�DA��A�hA
=A�7A+A�A�#A7LA��A�A&�A��AI�A
=A��A�A�A�A��A�A�A&�A	��A(�AC�AĜA  A�TA�hAn�A��A��A��AI�A��A�hAoA Q�@�ƨ@��@�M�@��7@��m@���@�j@��
@���@�K�@���@�l�@�J@�?}@�j@�@�5?@�9@�\)@�-@�1@�o@�9X@�\)@�5?@��y@ݩ�@�\)@ڏ\@ٺ^@؋D@ׅ@�{@ӥ�@�n�@ѩ�@�x�@�Q�@�S�@��@�/@̃@�\)@�o@ʧ�@ɡ�@���@Ȭ@�bN@�(�@�b@�K�@���@ũ�@��/@ċD@�v�@��j@�  @�n�@��u@�bN@�C�@�5?@�@�@��#@�7L@��@���@��@�{@��^@��7@��D@� �@��@���@���@�7L@��`@�Ĝ@���@�Z@�z�@���@��j@��@���@�l�@�t�@��@�I�@�bN@�9X@���@�1'@�l�@�  @�(�@�I�@�Z@�Q�@��R@�1@��@��w@��@��@�p�@��@���@��D@� �@�;d@���@��-@��7@�p�@�G�@�/@�Ĝ@�Q�@� �@��@��m@��P@�C�@��R@�$�@�`B@��@��@��j@��@�
=@��R@���@��@�ȴ@�V@��@��@���@�  @�9X@�Ĝ@�1'@�1@��
@��@��@�"�@���@�~�@�O�@��m@�o@��\@��#@�O�@��;@�\)@���@��@��m@�j@��@��\@�E�@��@���@���@�O�@��/@��u@�bN@�Q�@�I�@�1@�ƨ@���@��w@��w@���@���@��@�l�@�S�@�;d@��y@��y@���@�n�@�E�@�5?@���@���@���@�hs@�G�@�G�@�7L@��@���@���@���@�Ĝ@�%@��@���@��@��`@��@��@��/@���@��9@�Q�@�bN@�1'@�r�@�j@�dZ@�"�@�33@�"�@�;d@���@�l�@�K�@���@�"�@��H@�-@��T@�@�@�@��7@�p�@�G�@��@�r�@��F@�|�@�K�@��@���@��@��R@�v�@�v�@�~�@�v�@�$�@��@�1'@��u@y��@qhs@k"�@d1@\9X@SdZ@K�
@Fv�@?��@9G�@4�/@/�@(�`@#dZ@�+@hs@/@�\11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�VA�VA�VA�Q�A�I�A�n�A��`A��A���A��wA��jA��-A��!A��A���A���A���A���A���A��PA�%A��A��9A�?}A�ƨA�VA��A�(�A�  A���A��A�x�A��A�bNA��A���A���A�\)A�?}A�33A�&�A�A���A�n�A�C�A�33A�33A��A�hsA��A���A��yA��!A��!A��A�VA���A��jA��DA��A�dZA��A�I�A��A���A�|�A�p�A�^5A�=qA�%A��jA�jA�$�A��A�Q�A�ffA�A��HA�bNA���A�1A�33A�A�I�A�E�A��yA�9XA���A��jA���A�dZA�;dA�=qA�Q�A��uA�ffA�^5A�5?A���A�oA�~�A���A�VA��A��hA��AA~�uA{Az{Av�RAv1At�/Ar�RAo�^An�DAm`BAl(�Ai`BAh(�Ag��Ag&�Afn�Ac;dAa�A_��A\�A["�AW�^AVAUhsAT�ATM�ASC�ARjAQ�
AQ33AP1'AM
=AJ~�AI�
AH��AFbNAE��AD��AD=qAC��AC�FAC��AC�mAD�AC��AC��AC&�AB�\AA��AAx�A@ffA<�A:�A:$�A9ƨA8�A7oA6(�A5�mA4Q�A2�!A1�-A0z�A/��A.�9A.Q�A-�A-O�A,��A,�!A,��A,�\A,=qA*��A)�hA(v�A'�-A& �A$��A#�A#33A"��A"�A!��A �A�mA�DA��A�hA
=A�7A+A�A�#A7LA��A�A&�A��AI�A
=A��A�A�A�A��A�A�A&�A	��A(�AC�AĜA  A�TA�hAn�A��A��A��AI�A��A�hAoA Q�@�ƨ@��@�M�@��7@��m@���@�j@��
@���@�K�@���@�l�@�J@�?}@�j@�@�5?@�9@�\)@�-@�1@�o@�9X@�\)@�5?@��y@ݩ�@�\)@ڏ\@ٺ^@؋D@ׅ@�{@ӥ�@�n�@ѩ�@�x�@�Q�@�S�@��@�/@̃@�\)@�o@ʧ�@ɡ�@���@Ȭ@�bN@�(�@�b@�K�@���@ũ�@��/@ċD@�v�@��j@�  @�n�@��u@�bN@�C�@�5?@�@�@��#@�7L@��@���@��@�{@��^@��7@��D@� �@��@���@���@�7L@��`@�Ĝ@���@�Z@�z�@���@��j@��@���@�l�@�t�@��@�I�@�bN@�9X@���@�1'@�l�@�  @�(�@�I�@�Z@�Q�@��R@�1@��@��w@��@��@�p�@��@���@��D@� �@�;d@���@��-@��7@�p�@�G�@�/@�Ĝ@�Q�@� �@��@��m@��P@�C�@��R@�$�@�`B@��@��@��j@��@�
=@��R@���@��@�ȴ@�V@��@��@���@�  @�9X@�Ĝ@�1'@�1@��
@��@��@�"�@���@�~�@�O�@��m@�o@��\@��#@�O�@��;@�\)@���@��@��m@�j@��@��\@�E�@��@���@���@�O�@��/@��u@�bN@�Q�@�I�@�1@�ƨ@���@��w@��w@���@���@��@�l�@�S�@�;d@��y@��y@���@�n�@�E�@�5?@���@���@���@�hs@�G�@�G�@�7L@��@���@���@���@�Ĝ@�%@��@���@��@��`@��@��@��/@���@��9@�Q�@�bN@�1'@�r�@�j@�dZ@�"�@�33@�"�@�;d@���@�l�@�K�@���@�"�@��H@�-@��T@�@�@�@��7@�p�@�G�@��@�r�@��F@�|�@�K�@��@���@��@��R@�v�@�v�@�~�@�v�@�$�@��@�1'@��u@y��@qhs@k"�@d1@\9X@SdZ@K�
@Fv�@?��@9G�@4�/@/�@(�`@#dZ@�+@hs@/@�\11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB~�B~�B~�B}�B|�B�\B��B��BƨBƨBƨBƨBǮBȴBǮBǮB��B��B��B��B��BǮB�B��B��B�B�jBÖBƨB��B�wB�XB�3B�LB�RB�'B�!B�-B�qB�wB�wB�qB�jB�XB�LB�LB�FB�9B�B��B��B��B�oB�oB�hB�VB�%B�B�B{�Bw�Bq�Bn�Bm�Bk�Bk�Bl�Bm�Bn�Bl�Be`B\)BT�BM�B?}B#�B�B�BVB��B�sB��BÖB�FB��B}�BP�B<jB$�B�BĜB�B� BP�B=qB"�B
��B
��B
�dB
��B
�bB
�+B
�B
s�B
jB
_;B
Q�B
H�B
5?B
&�B
1B
  B	�B	�`B	��B	B	�^B	�'B	��B	�\B	�7B	�B	|�B	cTB	S�B	B�B	%�B	�B��B�B�B��B	PB		7B	B��B��B�B�#BĜBB�}BÖBŢBƨB��B��B��B�
B�NB�B��B	B	B	B	B	B	  B��B�B�B�sB�`B�BB�)B�B�B��B��BɺBŢB��B��B�}B�jB�dB�^B�XB�RB�LB�3B�B�B��B��B��B��B��B��B��B�uB�VB�JB�+B�B|�Bz�Bu�Bs�Bq�Bo�Bl�BjBhsBdZB`BB\)BXBS�BQ�BP�BN�BL�BE�BD�BB�B>wB7LB49B49B33B2-B1'B/B.B.B-B,B+B)�B)�B)�B(�B(�B(�B'�B'�B'�B&�B%�B%�B$�B#�B$�B!�B!�B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B"�B#�B-B-B,B,B-B.B0!B1'B1'B2-B2-B2-B2-B2-B2-B33B33B6FB7LB9XB9XB8RB;dB:^B9XB;dB=qB@�BB�BC�BD�BH�BJ�BO�BP�BS�BXBZBZB]/B`BBbNBcTBdZBffBk�Bm�Bo�Bo�Bp�Bs�Bz�B�B�uB��B��B��B�B�3B�RB�jB�}B��BÖB��B��B��B��B��B��B��B��B��B��B��B�
B�#B�/B�5B�5B�;B�BB�NB�sB�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B	%B	DB	VB	DB	\B	VB	hB	�B	�B	�B	!�B	#�B	$�B	%�B	&�B	'�B	&�B	$�B	&�B	&�B	%�B	$�B	#�B	!�B	"�B	%�B	(�B	-B	33B	33B	2-B	5?B	6FB	8RB	9XB	:^B	<jB	?}B	C�B	D�B	F�B	I�B	L�B	O�B	R�B	S�B	VB	XB	\)B	`BB	aHB	e`B	iyB	k�B	l�B	o�B	r�B	t�B	z�B	|�B	~�B	�B	�B	�B	�B	�%B	�+B	�1B	�=B	�PB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�'B	�!B	�3B	�FB	�LB	�XB	�jB	�qB	��B	B	ĜB	ƨB	ƨB	ŢB	ŢB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�B	�
B	�B	�
B	�/B	�B
B

=B
{B
�B
#�B
0!B
7LB
>wB
@�B
I�B
M�B
S�B
ZB
_;B
cTB
ffB
jB
m�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BB	B	B~ B|�B�jB��B��BƸBƶBƶBƹB��B��B��B��B��B��B��B��B�B��B�#B��B��B�B�~BèBƸB��B��B�fB�FB�\B�^B�7B�0B�:B��B��B��B��B�}B�jB�_B�^B�VB�IB� B�B��B��B�~B�~B�vB�cB�5B�&B�B{�Bw�Bq�Bn�Bm�Bk�Bk�Bl�Bm�Bn�Bl�BeoB\6BUBM�B?�B#�B�B�B`B��B�B��BáB�TB��B}�BP�B<vB$�B�BĥB�B�BP�B=B"�B
��B
��B
�qB
��B
�oB
�=B
�'B
s�B
j�B
_NB
Q�B
H�B
5QB
&�B
BB
 B	��B	�uB	��B	¥B	�uB	�=B	��B	�sB	�NB	�1B	}B	cmB	TB	B�B	%�B	�B�B�B��B��B	kB		TB	5B�B��B�B�CBļB­B��BöB��B��B��B��B�B�)B�kB��B�B	)B	4B	:B	<B	'B	 B��B�B�B�B�}B�`B�GB�7B�%B�B��B��B��B��B��B��B��B��B��B�wB�tB�mB�TB�7B�#B�B��B��B��B��B��B��B��B�wB�mB�QB�+B}B{Bu�Bs�Bq�Bo�Bl�Bj�Bh�BdyB`hB\OBX5BTBRBQ
BN�BL�BE�BD�BB�B>�B7rB4_B4FB3[B2TB1MB/BB.:B.:B-3B,B+B*$B*B*#B)B)B)B(B'�B(B'B&B&
B$�B#�B%B!�B!�B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B"�B$ B-4B-8B,/B,/B-3B.9B0JB1NB1PB2TB2TB2RB2VB2UB2SB3ZB3[B6jB7tB9�B9{B8xB;�B:�B9B;�B=�B@�BB�BC�BD�BH�BJ�BPBQ
BTBX5BZBBZBB]PB`dBbqBcvBd{Bf�Bk�Bm�Bo�Bo�Bp�Bs�B{B�*B��B��B��B��B�;B�RB�oB��B��B��BõB��B��B��B��B��B��B��B�
B�B�B�B�*B�CB�NB�TB�QB�ZB�aB�nB�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B� B	@B	_B	vB	aB	{B	sB	�B	�B	�B	�B	!�B	#�B	$�B	&B	'B	(
B	'B	$�B	'B	'B	%�B	$�B	#�B	!�B	"�B	%�B	)B	-*B	3NB	3OB	2GB	5XB	6`B	8mB	9tB	:yB	<�B	?�B	C�B	D�B	F�B	I�B	L�B	O�B	SB	TB	VB	X*B	\EB	`]B	aaB	eyB	i�B	k�B	l�B	o�B	r�B	t�B	z�B	}B	B	�%B	�*B	�*B	�0B	�<B	�EB	�JB	�UB	�jB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�9B	�<B	�9B	�LB	�\B	�cB	�oB	��B	��B	��B	¨B	ıB	ƾB	ƿB	ŷB	ŹB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�	B	�B	�B	�"B	�"B	�%B	�"B	�&B	�"B	�EB	�B
!B

OB
�B
�B
#�B
04B
7_B
>�B
@�B
I�B
M�B
T
B
Z0B
_NB
cfB
fwB
j�B
m�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.11 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311448582016053114485820160531144858  AO  ARCAADJP                                                                    20140721230640    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230640  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230640  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531144858  IP                  G�O�G�O�G�O�                