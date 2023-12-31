CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2017-12-01T18:05:12Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         (Matthew Alkire, University of Washington      @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7,   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  74   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7t   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     88   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8X   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8x   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           8|   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
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
_FillValue                    g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qt   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �\   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �\   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �P   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �$   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �(   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �,Argo profile    3.1 1.2 19500101000000  20171201180512  20190604094031  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4051                            2C  D   APEX                            5368                            041511                          846 @�9�2qF1   @�9��fuH@4�j~��#�d�fffff1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @�  @�  A   A   A@  A`  A���A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�33B���B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`�Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  DtffDyT{D� D�N�D���D��qD�HD�Q�D�~�D��RD�3D�I�D���D���D��D�7\Dڜ{D�)D�� D�PRD�{D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @x��@�z�@�z�A=qA>=qA^=qA�A��A��A��A��A��A��A��A��B�\B�\B�\B�\B'�\B/�\B7�\B?�\BG�\BO�\BW�\B_�\Bg�\Bo�\Bw�\B�\B�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB���B��{B�ǮB�ǮB�ǮB���B�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮC��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_�qCa��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D x�D ��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��D	x�D	��D
x�D
��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��D x�D ��D!x�D!��D"x�D"��D#x�D#��D$x�D$��D%x�D%��D&x�D&��D'x�D'��D(x�D(��D)x�D)��D*x�D*��D+x�D+��D,x�D,��D-x�D-��D.x�D.��D/x�D/��D0x�D0��D1x�D1��D2x�D2��D3x�D3��D4x�D4��D5x�D5��D6x�D6��D7x�D7��D8x�D8��D9x�D9��D:x�D:��D;x�D;��D<x�D<��D=x�D=��D>x�D>��D?x�D?��D@x�D@��DAx�DA��DBx�DB��DCx�DC��DDx�DD��DEx�DE��DFx�DF��DGx�DG��DHx�DH��DIx�DI��DJx�DJ��DKx�DK��DLx�DL��DMx�DM��DNx�DN��DOx�DO��DPx�DP��DQx�DQ��DRx�DR��DSx�DS��DTx�DT��DUx�DU��DVx�DV��DWx�DW��DXx�DX��DYx�DY��DZx�DZ��D[x�D[��D\x�D\��D]x�D]��D^x�D^��D_x�D_��D`x�D`��Dax�Da��Dbx�Db��Dcx�Dc��Ddx�Dd��Dex�De��Dfx�Df��Dgx�Dg��Dhx�Dh��Dix�Di��Djx�Dj��Dkx�Dk��Dlx�Dl��Dmx�Dm��Dnx�Dn��Dox�Do��Dpx�Dp��Dqx�Dq��Drx�Dr��Dsx�Ds��Dt_\DyMqD�{D�K3D�~gD���D��D�ND�{3D���D��D�FgD��qD��gD�gD�3�Dژ�DฤD��{D�L�D��D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�M�A�Q�A�O�A�O�A�O�A�Q�A�S�A�VA�S�A�S�A�ZA�^5A�ZA�XA�XA�ZA�S�A�E�A�I�A�G�A�9XA�1'A�1'A�/A�-A�-A�(�A�&�A�&�A��A��A�{A�bA�1A�  A���A���A���A���A�S�A�9XA�|�A�r�A��PA���A�VA��A��hA�=qA��-A�?}A��DA��A�l�A�ȴA�x�A�1'A���A�XA�7LA��A�Q�A�/A�A���A��PA�^5A��RA�/A�M�A��HA�ĜA�
=A���A�K�A�1A��PA�JA�`BA��A��jA��/A��RA��yA�x�A��A�ffA�A��jA�z�A�XA��uA�5?A���A�
=A�"�A��DA�&�A���A�+A��yA���A��A���A���A�K�A�ffA���A��DA�"�A�VA��9A�A���A��A�A�A��A?}A~^5A}��A|r�AydZAv��At�Arn�Ap��AnjAm"�AkS�Ah��Ag/AfȴAe�FAehsAdM�Ac/A`jA_�^A_%A\��A["�AZ^5AV�DAU"�AT~�AT-AS�7AQƨAP1'AN�DAM��AL��AL=qAK�hAI��AF��AEx�ADA�AB$�AA��AA7LA@ĜA@�!A@A>~�A=VA;�hA:��A:ffA9l�A8�A7�hA69XA3��A1�A/�A/x�A-&�A+�A+��A*�HA(�yA'�TA&�/A%�A$�jA#ƨA"��A"Q�A!��A!�wA!�A bNAhsAƨA�!AO�AM�A��AoA�+AbA��AȴA��Av�A��A"�AVA"�AS�A1'A
�HA��A�^A�RAdZA$�A+A^5Ap�A ��@��@�(�@��+@��@��;@���@�9X@��y@�9@�t�@�^5@�7L@�I�@��@�G�@�r�@�@�-@�Ĝ@�o@���@�r�@�b@ޗ�@��y@ڗ�@׍P@ա�@�o@У�@�(�@ύP@��@���@�O�@˾w@�"�@�"�@��H@ʸR@ʧ�@���@�S�@�  @��@ˍP@��@�Q�@�$�@�  @�1'@�v�@���@�ȴ@��@��y@�33@�"�@�=q@�I�@�  @��u@���@�ȴ@�ƨ@�
=@��-@��-@�@�X@��@�
=@��`@���@��@�;d@��P@���@�"�@�I�@��+@��-@���@�j@�+@���@��@���@�O�@�%@��/@��D@�Z@�A�@�1'@�l�@��R@�$�@���@�Z@�  @���@�;d@���@���@���@�ȴ@��H@���@�v�@�^5@�{@��7@�7L@�V@��@�r�@��w@�33@��H@�@�S�@��w@�|�@�t�@�S�@�@���@���@�ȴ@�
=@�"�@��@�ȴ@���@�33@��R@�M�@���@�?}@���@��@�bN@�(�@�1'@�1@��m@�ƨ@�33@��y@�~�@�E�@��@��#@��-@�hs@�X@��7@��h@�O�@�&�@��@�%@���@�C�@�ff@�ff@�V@�E�@�E�@�5?@���@�/@�Ĝ@�1'@�Z@��@��w@�o@���@�5?@���@�@���@�X@�?}@��@���@�j@�A�@� �@��@��m@��@���@���@�|�@�C�@���@�^5@�=q@�-@��@���@�p�@�?}@�&�@���@��9@�bN@�9X@�1@���@�t�@�K�@�+@�@��@���@�ȴ@��\@�n�@�V@�E�@�-@��@���@���@�x�@�hs@�`B@�G�@�&�@��/@���@�r�@�Z@�A�@�b@��
@���@�K�@�"�@��@���@�$�@�J@��#@���@�`B@�/@�V@�%@��@���@��@�I�@� �@��;@�t�@�C�@�o@��@�V@�@��-@�q@��@xD�@q5�@i�9@b	@[�:@SS�@L@DFt@=��@5��@-��@'��@"��@��@@�o@:�@�@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A�M�A�Q�A�O�A�O�A�O�A�Q�A�S�A�VA�S�A�S�A�ZA�^5A�ZA�XA�XA�ZA�S�A�E�A�I�A�G�A�9XA�1'A�1'A�/A�-A�-A�(�A�&�A�&�A��A��A�{A�bA�1A�  A���A���A���A���A�S�A�9XA�|�A�r�A��PA���A�VA��A��hA�=qA��-A�?}A��DA��A�l�A�ȴA�x�A�1'A���A�XA�7LA��A�Q�A�/A�A���A��PA�^5A��RA�/A�M�A��HA�ĜA�
=A���A�K�A�1A��PA�JA�`BA��A��jA��/A��RA��yA�x�A��A�ffA�A��jA�z�A�XA��uA�5?A���A�
=A�"�A��DA�&�A���A�+A��yA���A��A���A���A�K�A�ffA���A��DA�"�A�VA��9A�A���A��A�A�A��A?}A~^5A}��A|r�AydZAv��At�Arn�Ap��AnjAm"�AkS�Ah��Ag/AfȴAe�FAehsAdM�Ac/A`jA_�^A_%A\��A["�AZ^5AV�DAU"�AT~�AT-AS�7AQƨAP1'AN�DAM��AL��AL=qAK�hAI��AF��AEx�ADA�AB$�AA��AA7LA@ĜA@�!A@A>~�A=VA;�hA:��A:ffA9l�A8�A7�hA69XA3��A1�A/�A/x�A-&�A+�A+��A*�HA(�yA'�TA&�/A%�A$�jA#ƨA"��A"Q�A!��A!�wA!�A bNAhsAƨA�!AO�AM�A��AoA�+AbA��AȴA��Av�A��A"�AVA"�AS�A1'A
�HA��A�^A�RAdZA$�A+A^5Ap�A ��@��@�(�@��+@��@��;@���@�9X@��y@�9@�t�@�^5@�7L@�I�@��@�G�@�r�@�@�-@�Ĝ@�o@���@�r�@�b@ޗ�@��y@ڗ�@׍P@ա�@�o@У�@�(�@ύP@��@���@�O�@˾w@�"�@�"�@��H@ʸR@ʧ�@���@�S�@�  @��@ˍP@��@�Q�@�$�@�  @�1'@�v�@���@�ȴ@��@��y@�33@�"�@�=q@�I�@�  @��u@���@�ȴ@�ƨ@�
=@��-@��-@�@�X@��@�
=@��`@���@��@�;d@��P@���@�"�@�I�@��+@��-@���@�j@�+@���@��@���@�O�@�%@��/@��D@�Z@�A�@�1'@�l�@��R@�$�@���@�Z@�  @���@�;d@���@���@���@�ȴ@��H@���@�v�@�^5@�{@��7@�7L@�V@��@�r�@��w@�33@��H@�@�S�@��w@�|�@�t�@�S�@�@���@���@�ȴ@�
=@�"�@��@�ȴ@���@�33@��R@�M�@���@�?}@���@��@�bN@�(�@�1'@�1@��m@�ƨ@�33@��y@�~�@�E�@��@��#@��-@�hs@�X@��7@��h@�O�@�&�@��@�%@���@�C�@�ff@�ff@�V@�E�@�E�@�5?@���@�/@�Ĝ@�1'@�Z@��@��w@�o@���@�5?@���@�@���@�X@�?}@��@���@�j@�A�@� �@��@��m@��@���@���@�|�@�C�@���@�^5@�=q@�-@��@���@�p�@�?}@�&�@���@��9@�bN@�9X@�1@���@�t�@�K�@�+@�@��@���@�ȴ@��\@�n�@�V@�E�@�-@��@���@���@�x�@�hs@�`B@�G�@�&�@��/@���@�r�@�Z@�A�@�b@��
@���@�K�@�"�@��@���@�$�@�J@��#@���@�`B@�/@�V@�%@��@���@��@�I�@� �@��;@�t�@�C�@�o@��@�V@�G�O�@�q@��@xD�@q5�@i�9@b	@[�:@SS�@L@DFt@=��@5��@-��@'��@"��@��@@�o@:�@�@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBx�Bx�Bx�Bx�Bx�Bx�Bx�Bx�Bx�Bx�Bx�Bx�By�Bz�Bz�By�Bz�B~�B~�B~�B�B�B�B�B�B�B�B�B�B�%B�%B�+B�+B�+B�+B�+B�+B�%B�B� B�Bx�B�B�B�1B�+B�+B�=B�=B�JB�VB�hB��B��B��B��B��B��B�B�B�B�RB�^B�dB�qB�jB�3B��B��B��B�hB�JB�=B�7B�+B�%B� By�Bq�BhsB]/BO�B?}B33B+B$�B�BoBB��B��B�BB��B�'B�JB�DB�{B�bB�+Bu�Bv�Bv�BaHBN�B1'BuBB
�fB
��B
�jB
�B
��B
�bB
� B
n�B
T�B
F�B
8RB
1'B
,B
�B
%B	�B	�TB	��B	ɺB	�^B	�RB	�?B	�FB	�FB	�wB	�FB	�'B	�-B	�B	��B	��B	��B	�PB	�B	{�B	o�B	gmB	dZB	aHB	]/B	T�B	L�B	E�B	B�B	D�B	A�B	=qB	1'B	�B	�B	hB	
=B	+B	B	B	B��B��B�B�B�B�yB�fB�TB�5B�B��B��BŢBB�XB�9B�-B�B��B��B��B��B��B��B�uB�oB�hB�bB�PB�DB�1B�B� B{�Bw�Bu�Bs�Bq�Bp�Bo�Bm�BjBiyBgmBffBe`BdZB`BB^5B\)B^5B^5B]/B[#B\)B^5B^5B`BB^5B[#BZB[#B`BBbNB`BB_;B^5B_;BbNBaHBe`BffBe`BcTBaHBcTBe`Be`BjBm�Bm�Bm�Bl�Bp�BgmBbNB\)BZBT�BR�BP�BO�BN�BM�BN�BR�BT�BW
BZB]/BaHBk�Bs�Bv�B�B�B�B}�Bs�Br�Bx�B~�B~�B~�B� B�B�B� By�B{�B�B�\B��B��B��B��B��B��B�B�B�B�B�'B�FB�qBŢBƨBɺB��BɺB��B��B��B��B�
B�B�B�)B�5B�;B�HB�NB�ZB�ZB�B�B�B��B��B	B	1B	DB	VB	oB	{B	�B	�B	�B	!�B	%�B	(�B	,B	/B	1'B	2-B	5?B	:^B	?}B	C�B	H�B	M�B	T�B	W
B	YB	ZB	\)B	]/B	^5B	bNB	e`B	gmB	iyB	jB	m�B	t�B	x�B	z�B	{�B	z�B	z�B	{�B	|�B	}�B	�B	�B	�B	�B	�JB	�\B	�\B	�bB	�oB	�uB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�-B	�9B	�LB	�XB	�^B	�jB	�qB	��B	ƨB	ǮB	ǮB	ȴB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�B	�B	�B	�B	�B	�#B	�)B	�)B	�/B	�/B	�;B	�HB	�TB	�ZB	�`B	�`B	�`B	�fB	�mB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
�B
�B
!�B
+�B
5tB
;B
@�B
GB
K�B
OvB
TB
Z�B
abB
g�B
n�B
tnB
x�B
zDB
}B
�B
�+11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   Bk�Bk�Bk�Bk�Bk�Bk�Bk�Bk�Bk�Bk�Bk�Bk�Bl�Bm�Bm�Bl�Bm�BrBrBrBu*Bv'Bw3Bw-Bw-Bw0Bx:Bx5Bx8By?By?BzEBzJBzEBz@BzEBzCBy<Bx:BsBw2Bk�Bt%Bx=B{PBzJBzNB}UB}WBdB�rB��B��B��B��B��B��B�B�B�#B�*B�mB�}B�}B��B��B�MB�	B��B��B��BlB}[B|VBzNByBBsBl�Bd�B[�BPRBCB2�B&ZB.B	B�B�B�MB�B��B�vB��B�bB�B~�B��B��BziBiBj	BjBT�BBB$tB�B
�UB
ٱB
�HB
��B
�bB
�B
��B
sZB
a�B
H\B
:
B
+�B
$�B
mB
B	��B	�B	��B	�eB	�+B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�	B	�B	��B	w�B	ocB	cB	Z�B	W�B	T�B	P�B	H}B	@NB	9$B	6B	8B	5B	0�B	$�B	AB	B	�B��B��B��B��B��B�B�]B�7B�B�B�B��B��B��B̦B�vB�QB�4B�$B��B��B��B��B�|B�pB�oB�CB�+B�B�B�B�B��B��B~�B{�Bv�Bs�Bo�BknBiaBgVBeIBdCBcABa.B^"B]B[BZBYBX BS�BQ�BO�BQ�BQ�BP�BN�BO�BQ�BQ�BS�BQ�BN�BM�BN�BS�BU�BS�BR�BQ�BR�BU�BT�BYBZBYBV�BT�BV�BYBYB^&Ba4Ba6Ba8B`1BdMB[BU�BO�BM�BH�BF�BD�BC�BB�BABB�BF�BH�BJ�BM�BP�BT�B_/Bg_BjqBt�Bv�Bw�Bq�Bg_BfVBl}Br�Br�Br�Bs�Bu�Bu�Bs�Bm�Bo�Bw�B�B�9B�bB�B�yB��B��B��B��B��B��B��B��B�B�FB�GB�^B�]B�_B�mB�hB�xBƔBʫB˳B̹B��B��B��B��B��B��B��B�%B�<B�KB�bB�~B��B��B��B	�B	B	B	
"B	,B	HB	iB	|B	�B	�B	"�B	$�B	%�B	(�B	-�B	3B	7.B	<OB	AhB	H�B	J�B	L�B	M�B	O�B	P�B	Q�B	U�B	X�B	[B	]B	^B	a)B	hRB	lgB	nuB	o{B	ntB	nuB	ozB	p�B	q�B	t�B	v�B	v�B	w�B	�B	��B	��B	��B	�B	�	B	�B	�B	�B	�.B	�6B	�MB	�_B	�\B	�ZB	�]B	�]B	�VB	�bB	�bB	�aB	�`B	�kB	�iB	�dB	�\B	�VB	�pB	�oB	�iB	�vB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�6B	�=B	�=B	�@B	�;B	�CB	�GB	�SB	�ZB	�bB	�nB	�pB	�tB	�xB	�zB	�xB	ƁB	ȉB	ȊB	ʖB	ʕB	˞B	˝B	̦B	̥B	̥B	αB	ϴB	ϷB	йB	жB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�	B	�B	�B	�B	�!B	�$B	�)B	�+B	�;B	�9B	�AB	�HB	�PB	�NB	�LB	�OB	�SB	�UB	�ZB	�^B	�`B	�jB	�oB	�{B	�B	�B	�B	��G�O�B	�SB
3B
PB
wB
(�B
.�B
4=B
:�B
?MB
B�B
G�B
N>B
T�B
[?B
b4B
g�B
lB
m�B
p�B
smB
z�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.11 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9997(+/-0), vertically averaged dS =-0.012(+/-0.002) in PSS-78.                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Map scales: x=6,3; y=2,1. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                            201906040940312019060409403120190604094031  AO  ARCAADJP                                                                    20171201180512    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20171201180512  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20171201180512  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604094031  IP                  G�O�G�O�G�O�                