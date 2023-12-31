CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:06:35Z AOML 3.0 creation; 2016-05-31T21:48:57Z UW 3.1 conversion     
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
resolution        :�o     �  U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gx   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ix   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qp   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �`   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �X   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �(   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �,   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �0   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20140721230635  20160531144857  5903738 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  4053_7107_001                   2C  D   APEX                            5370                            041511                          846 @�,ܵڿ�1   @�,�N�@ @7޸Q��c-�E��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @�ff@�  A   A   A@  A`  A���A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=�fD>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDy� D��D�L�D���D�� D�  D�I�D�s3D���D�fD�S3D��3DǶfD� D�C3Dڀ D� D�  D�Y�D� D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��G@�z�@�z�A=qA>=qA^=qA�A��A��A��A��A��A��A��A��B�\B�\B�\B�\B'�\B/�\B7�\B?�\BG�\BO�\BW�\B_�\Bg�\Bo�\Bw�\B�\B�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮC��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D x�D ��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��D	x�D	��D
x�D
��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��D\D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��D x�D ��D!x�D!��D"x�D"��D#x�D#��D$x�D$��D%x�D%��D&x�D&��D'x�D'��D(x�D(��D)x�D)��D*x�D*��D+x�D+��D,x�D,��D-x�D-��D.x�D.��D/x�D/��D0x�D0��D1x�D1��D2x�D2��D3x�D3��D4x�D4��D5x�D5��D6x�D6��D7x�D7��D8x�D8��D9x�D9��D:x�D:��D;x�D;��D<x�D<��D=\D=��D>x�D>��D?x�D?��D@x�D@��DAx�DA��DBx�DB��DCx�DC��DDx�DD��DEx�DE��DFx�DF��DGx�DG��DHx�DH��DIx�DI��DJx�DJ��DKx�DK��DLx�DL��DMx�DM��DNx�DN��DOx�DO��DPx�DP��DQx�DQ��DRx�DR��DSx�DS��DTx�DT��DUx�DU��DVx�DV��DWx�DW��DXx�DX��DYx�DY��DZx�DZ��D[x�D[��D\x�D\��D]x�D]��D^x�D^��D_x�D_��D`x�D`��Dax�Da��Dbx�Db��Dcx�Dc��Ddx�Dd��Dex�De��Dfx�Df��Dgx�Dg��Dhx�Dh��Dix�Di��Djx�Dj��Dkx�Dk��Dlx�Dl��Dmx�Dm��Dnx�Dn��Dox�Do��Dpx�Dp��Dqx�Dq��Drx�Dr��Dsx�Ds��Dtx�Dt�\Dyx�D�	HD�IHD��D��{D��{D�FD�o�D��D��D�O�D���Dǲ�D�{D�?�D�|{D�{D��{D�VD�{D��H111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�-A�&�A�(�A�+A�+A�-A�$�A� �A���A���A��9A��A���A���A��hA��DA��A�ffA�^5A�VA�VA�VA�^5A�ZA�O�A�VA�Q�A�K�A�C�A�=qA�9XA�5?A�1'A�-A�+A�-A�&�A�&�A�&�A�$�A�"�A���A��/A���A�ȴA��A���A���A��`A��A��\A��`A���A��wA�G�A�+A�I�A�G�A��RA���A� �A�l�A�ȴA�C�A���A�5?A��HA�ȴA��A�9XA���A�K�A��9A�A���A��TA�`BA�%A��DA���A���A�z�A�
=A���A��A��-A�jA�A�7LA��9AA~��A|�/AyAu��Aq
=AmC�Aj��An{Ap5?Al�Al{AkK�Aj��AjbAh�HAg�AfQ�Ae�TAe�Ac�Ab�9AaO�A`z�A_�wA_\)A]|�A[p�AY�FAY%AW�AV��AU�AU;dATjAS�ASVAQ��AQAP=qAOO�AN{AM`BAL1'AK|�AJȴAI�AH�DAHAG�7AF�RAD�RAC�AC33ABM�AAt�A@��A?/A>�yA>�jA=�A<n�A;��A:r�A9��A8�A7�mA7�^A7��A7�7A5��A4��A4 �A3&�A2E�A1%A0z�A/�A/C�A.��A-�^A-+A,ĜA,�DA+�^A)�wA'�7A&�\A&�DA&ZA$�A$(�A#��A#l�A#A"5?A!��A ��A �A�AE�A�
AJAx�A�A33AdZAhsAI�A��A"�A�A��A�uA��At�A�A
��A	�A	��A	
=AhsA~�A33A�A��A�jA�TA �@�;d@��H@�5?@�z�@�+@�ff@�@�|�@���@���@��m@��H@�Z@�R@��@�b@��T@��`@�I�@�P@柾@��m@�~�@���@��m@��y@��/@���@ڰ!@٩�@�r�@և+@�O�@���@��@�&�@�  @���@�O�@�(�@ˍP@�\)@�J@�j@���@Ə\@�G�@ċD@��@�dZ@\@�&�@�\)@���@���@�S�@�E�@�X@�A�@���@�-@�{@��@�@��D@��@���@�j@��@�dZ@�$�@��^@�x�@��@�z�@��@��y@��@�@�G�@���@���@�l�@�dZ@�;d@�;d@�
=@��H@���@�ff@���@��@�K�@�
=@���@���@���@��y@���@�ȴ@��!@�~�@�V@�-@�-@�V@��@��9@�r�@�(�@��;@��@���@��@�K�@���@���@�=q@���@���@���@�X@�V@���@�  @��H@�^5@�M�@�M�@�n�@���@���@��@�@���@���@��-@��^@�`B@�&�@�Ĝ@�9X@� �@�A�@��@�t�@��@�~�@���@�hs@�p�@�{@��@�@��@�x�@�`B@�&�@�V@��@�Ĝ@��@�bN@�I�@� �@�1@�  @�S�@�"�@�;d@��y@���@�V@�@��T@�x�@�hs@�`B@�X@��@��@��@��@�j@� �@���@��@�C�@�
=@��@��@��H@���@�v�@�ff@�E�@�-@��@��-@���@�x�@�X@�/@��@���@�z�@�A�@�1@�b@��m@�;d@��@���@��y@���@���@���@�~�@�V@��^@��@��@��/@��`@���@��@��@�z�@�z�@�A�@�(�@��@�  @��m@���@�ƨ@���@��
@���@��P@��H@�ȴ@��R@���@�V@�{@�@��^@�p�@�X@�G�@�X@�G�@�V@�7L@�@��T@��@���@�r�@��;@��@�dZ@�33@�o@��H@��!@�~�@�n�@�E�@�A�@x  @m�T@fv�@_+@XbN@R�\@N��@IG�@B��@=�@8�@1hs@-V@(�`@%?}@ bN@�D@b@��@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�-A�&�A�(�A�+A�+A�-A�$�A� �A���A���A��9A��A���A���A��hA��DA��A�ffA�^5A�VA�VA�VA�^5A�ZA�O�A�VA�Q�A�K�A�C�A�=qA�9XA�5?A�1'A�-A�+A�-A�&�A�&�A�&�A�$�A�"�A���A��/A���A�ȴA��A���A���A��`A��A��\A��`A���A��wA�G�A�+A�I�A�G�A��RA���A� �A�l�A�ȴA�C�A���A�5?A��HA�ȴA��A�9XA���A�K�A��9A�A���A��TA�`BA�%A��DA���A���A�z�A�
=A���A��A��-A�jA�A�7LA��9AA~��A|�/AyAu��Aq
=AmC�Aj��An{Ap5?Al�Al{AkK�Aj��AjbAh�HAg�AfQ�Ae�TAe�Ac�Ab�9AaO�A`z�A_�wA_\)A]|�A[p�AY�FAY%AW�AV��AU�AU;dATjAS�ASVAQ��AQAP=qAOO�AN{AM`BAL1'AK|�AJȴAI�AH�DAHAG�7AF�RAD�RAC�AC33ABM�AAt�A@��A?/A>�yA>�jA=�A<n�A;��A:r�A9��A8�A7�mA7�^A7��A7�7A5��A4��A4 �A3&�A2E�A1%A0z�A/�A/C�A.��A-�^A-+A,ĜA,�DA+�^A)�wA'�7A&�\A&�DA&ZA$�A$(�A#��A#l�A#A"5?A!��A ��A �A�AE�A�
AJAx�A�A33AdZAhsAI�A��A"�A�A��A�uA��At�A�A
��A	�A	��A	
=AhsA~�A33A�A��A�jA�TA �@�;d@��H@�5?@�z�@�+@�ff@�@�|�@���@���@��m@��H@�Z@�R@��@�b@��T@��`@�I�@�P@柾@��m@�~�@���@��m@��y@��/@���@ڰ!@٩�@�r�@և+@�O�@���@��@�&�@�  @���@�O�@�(�@ˍP@�\)@�J@�j@���@Ə\@�G�@ċD@��@�dZ@\@�&�@�\)@���@���@�S�@�E�@�X@�A�@���@�-@�{@��@�@��D@��@���@�j@��@�dZ@�$�@��^@�x�@��@�z�@��@��y@��@�@�G�@���@���@�l�@�dZ@�;d@�;d@�
=@��H@���@�ff@���@��@�K�@�
=@���@���@���@��y@���@�ȴ@��!@�~�@�V@�-@�-@�V@��@��9@�r�@�(�@��;@��@���@��@�K�@���@���@�=q@���@���@���@�X@�V@���@�  @��H@�^5@�M�@�M�@�n�@���@���@��@�@���@���@��-@��^@�`B@�&�@�Ĝ@�9X@� �@�A�@��@�t�@��@�~�@���@�hs@�p�@�{@��@�@��@�x�@�`B@�&�@�V@��@�Ĝ@��@�bN@�I�@� �@�1@�  @�S�@�"�@�;d@��y@���@�V@�@��T@�x�@�hs@�`B@�X@��@��@��@��@�j@� �@���@��@�C�@�
=@��@��@��H@���@�v�@�ff@�E�@�-@��@��-@���@�x�@�X@�/@��@���@�z�@�A�@�1@�b@��m@�;d@��@���@��y@���@���@���@�~�@�V@��^@��@��@��/@��`@���@��@��@�z�@�z�@�A�@�(�@��@�  @��m@���@�ƨ@���@��
@���@��P@��H@�ȴ@��R@���@�V@�{@�@��^@�p�@�X@�G�@�X@�G�@�V@�7L@�@��T@��@���@�r�@��;@��@�dZ@�33@�o@��H@��!@�~�@�n�G�O�@�A�@x  @m�T@fv�@_+@XbN@R�\@N��@IG�@B��@=�@8�@1hs@-V@(�`@%?}@ bN@�D@b@��@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�RB�XB�XB�RB�XB�RB�RB�XB�}B��BBBÖBĜBĜBĜBÖB��B��B��B��BBĜBÖBBĜBĜBĜBÖBÖBÖBÖBÖBÖBĜBŢBĜBĜBŢBĜBĜBB��B�wB�jB�^B�3B��B�JB�B|�B_;B�yB�-B�DBC�B,B{B\BB�B�5B��BƨB�9B�oB~�Bk�Be`B]/BK�B�B
��B
�mB
��B
ǮB
��B
ffB
:^B
 �B	��B
B
#�B
I�B
t�B
33B
�B
�B
hB
	7B	��B	�B	�NB	�}B	��B	q�B	O�B	D�B	�DB	�wB	��B	�9B	�3B	�!B	��B	��B	��B	�hB	�\B	�DB	�B	~�B	z�B	v�B	r�B	n�B	cTB	\)B	Q�B	R�B	R�B	H�B	J�B	K�B	H�B	E�B	>wB	8RB	9XB	8RB	5?B	0!B	/B	-B	)�B	&�B	$�B	!�B	�B	�B	�B	{B	\B	
=B	B	B	%B	  B��B��B�B�fB�NB�
B��BǮBƨB��B��B�#B�B�
B��B��B��BƨB��B�wB�wB�qB�FB�-B�!B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�uB�VB�1B�Bx�BhsB]/B^5Be`Bm�Bl�BiyBgmBe`BdZB^5BS�BK�BC�B>wB<jB:^B6FB1'B.B,B)�B(�B'�B&�B'�B/B5?B6FB7LB9XB9XB7LB49B33B/B+B.B-B'�B$�B"�B�B�B�B�B�B�B�B{BoBoBbB\BVBPBJBJBDBDBJBPBVBJBDBPBVBPBPB\B\B\BhBhBhBhBoBuB�B�B�B�B�B�B"�B$�B&�B&�B&�B%�B)�B/B:^B=qB>wBA�BH�BJ�BK�BM�BN�BQ�BZB]/B^5B`BBbNBn�Bu�Bw�Bz�B}�B�B�JB�uB��B��B��B��B��B��B��B�B�!B�3B�?B�?B�FB�FB�XB�dB�wB�}BŢBǮBƨBƨBƨBƨBǮBɺB��B��B�B�B�B�/B�BB�`B�B�B�B�B�B�B�B��B��B��B��B	B	1B	JB	PB	bB	{B	�B	�B	�B	!�B	&�B	&�B	'�B	+B	,B	-B	1'B	9XB	<jB	>wB	?}B	B�B	F�B	G�B	H�B	L�B	P�B	R�B	S�B	S�B	S�B	S�B	S�B	S�B	W
B	]/B	bNB	e`B	ffB	ffB	gmB	jB	k�B	l�B	m�B	n�B	p�B	r�B	w�B	y�B	{�B	}�B	~�B	�B	�%B	�+B	�+B	�1B	�7B	�=B	�DB	�JB	�PB	�VB	�\B	�hB	�uB	�{B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�!B	�'B	�LB	�RB	�XB	�dB	�qB	�wB	�}B	�}B	�}B	�}B	��B	ÖB	ĜB	ĜB	ĜB	ŢB	ƨB	ȴB	ȴB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	�
B	�B	��B	��B	��B	��B	��B	��B	�
B	�
B	�B	�B	�)B	�TB	�TB	��B
%B
oB
�B
#�B
)�B
.B
49B
;dB
A�B
F�B
M�B
Q�B
XB
[#B
`BB
dZB
gmB
jB
o�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B�bB�fB�fB�bB�jB�bB�aB�gB��B��B¡B¡BçBıBĮBīBéB��B��B��B��BBĭBêB£BĭBĭBĩBçBçBéBéBæBæBįBŴBĭBĭBųBįBĭBB��B��B�zB�pB�DB��B�ZB�*B|�B_IB�B�7B�KBC�B,B�BcB-B�B�?B��BƳB�BB�zBBk�BelB]9BK�B�B
��B
�~B
��B
ǼB
��B
fwB
:pB
 �B	��B
B
#�B
I�B
t�B
3EB
�B
�B
{B
	IB	��B	�B	�_B	��B	��B	q�B	O�B	D�B	�\B	��B	��B	�PB	�IB	�6B	��B	��B	��B	��B	�rB	�\B	�5B	B	z�B	v�B	r�B	n�B	cmB	\DB	RB	SB	SB	H�B	J�B	K�B	H�B	E�B	>�B	8jB	9sB	8nB	5YB	0=B	/6B	-)B	*B	'B	$�B	!�B	�B	�B	�B	�B	|B	
ZB	;B	7B	AB	 B�B��B�B�B�lB�*B��B��B��B��B�B�AB�$B�)B�B�B��B��B��B��B��B��B�hB�MB�AB�4B�B��B��B��B��B�B�B�B��B��B��B��B��B��B��B�xB�UB�6Bx�Bh�B]SB^YBe�Bm�Bl�Bi�Bg�Be�Bd~B^ZBTBK�BC�B>�B<�B:�B6kB1NB.9B,.B*#B)B'�B'B'�B/BB5eB6jB7rB9~B9fB7qB4^B3>B/AB++B.9B-5B(B%B"�B�B�B�B�B�B�B�B�B�B�B�B�BdB[BWBWBRBmBWB_BcBWBnB_BeB]B]B�B�B�BsBtB�BvB�B�B�B�B�B�B�B�B"�B%B'B'B&�B&	B*#B/AB:�B=�B>�BA�BH�BJ�BK�BM�BO BRBZAB]TB^ZB`eBbrBn�Bu�Bw�B{B~B�<B�lB��B��B��B��B��B�B�B�B�%B�@B�SB�_B�aB�fB�gB�uB��B��B��B��B��B��B��B��B��B��B��B��B�B�&B�0B�=B�LB�`B�~B�B�B�B�B�B��B��B��B�B�B�B	)B	NB	eB	lB	~B	�B	�B	�B	�B	!�B	'B	'B	(B	+B	,&B	-'B	1CB	9rB	<�B	>�B	?�B	B�B	F�B	G�B	H�B	L�B	P�B	SB	TB	TB	TB	TB	TB	TB	W$B	]GB	beB	eyB	f�B	f~B	g�B	j�B	k�B	l�B	m�B	n�B	p�B	r�B	w�B	y�B	|B	~B	B	�#B	�>B	�BB	�BB	�JB	�OB	�VB	�\B	�cB	�hB	�nB	�tB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�3B	�4B	�2B	�7B	�9B	�<B	�cB	�gB	�nB	�}B	��B	��B	��B	��B	��B	��B	��B	ìB	ĴB	ĳB	ĵB	ŹB	ƽB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	� B	�B	�B	�B	�B	�B	�B	�B	�!B	� B	�'B	�5B	�?G�O�B	�iB	��B
:B
�B
�B
#�B
*B
.(B
4MB
;xB
A�B
F�B
M�B
R B
X"B
[8B
`UB
djB
g�B
j�B
o�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.11 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311448572016053114485720160531144857  AO  ARCAADJP                                                                    20140721230635    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230635  QCP$                G�O�G�O�G�O�8FB7E           AO  ARGQQCPL                                                                    20140721230635  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531144857  IP                  G�O�G�O�G�O�                