CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-05-24T22:00:42Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         (Matthew Alkire, University of Washington      @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
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
resolution        :�o     �  q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �|   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �|   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �t   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
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
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20180524220042  20190604094146  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4051                            2C  D   APEX                            5368                            041511                          846 @�e/"R�1   @�e/��E�@6� ě���d|�hs1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @333@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C�C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�Dy!�D�)D�O\D�qHD��)D�  D�FfD�o\D�ÅD�  D�:�D��\Dǿ
D�3D�8 D�uD���D�{D�H�D�3D��{111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @,(�@x��@�z�@�z�A=qA>=qA^=qA~=qA��A��A��A��A��A��A��A��B�\B�\B�\B�\B'�\B/�\B7�\B?�\BG�\BO�\BW�\B_�\Bg�\Bo�\Bw�\B�\B�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮC��C��C��C��C	��C��C��C��C�qC��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D x�D ��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��D	x�D	��D
x�D
��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��D x�D ��D!x�D!��D"x�D"��D#x�D#��D$x�D$��D%x�D%��D&x�D&��D'x�D'��D(x�D(��D)x�D)��D*x�D*��D+x�D+��D,x�D,��D-x�D-��D.x�D.��D/x�D/��D0x�D0��D1x�D1��D2x�D2��D3x�D3��D4x�D4��D5x�D5��D6x�D6��D7x�D7��D8x�D8��D9x�D9��D:x�D:��D;x�D;��D<x�D<��D=x�D=��D>x�D>��D?x�D?��D@x�D@��DAx�DA��DBx�DB��DCx�DC��DDx�DD��DEx�DE��DFx�DF��DGx�DG��DHx�DH��DIx�DI��DJx�DJ��DKx�DK��DLx�DL��DMx�DM��DNx�DN��DOx�DO��DPx�DP��DQx�DQ��DRx�DR��DSx�DS��DTx�DT��DUx�DU��DVx�DV��DWx�DW��DXx�DX��DYx�DY��DZx�DZ��D[x�D[��D\x�D\��D]x�D]��D^x�D^��D_x�D_��D`x�D`��Dax�Da��Dbx�Db��Dcx�Dc��Ddx�Dd��Dex�De��Dfx�Df��Dgx�Dg��Dhx�Dh��Dix�Di��Djx�Dj��Dkx�Dk��Dlx�Dl��Dmx�Dm��Dnx�Dn��Dox�Do��Dpx�Dp��Dqx�Dq��Drx�Dr��Dsx�Ds��Dtr�Dy�D��D�K�D�m�D���D��{D�B�D�k�D�� D�{D�7\D���Dǻ�D���D�4{D�q�D��qD��D�EqD��D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�^5A�^5A�^5A�^5A�`BA�`BA�bNA�`BA�`BA�`BA�^5A�bNA�^5A�E�Aƴ9A�ƨA�XA���A��hA�I�A�+A�ȴA�C�A��A��!A���A���A���A�hsA��A��A�A��PA�ffA�"�A���A��+A�-A�"�A���A��+A�Q�A�oA��/A�ƨA���A�7LA��A�VA�(�A��A��A���A�/A��A�1A���A��jA�/A��-A�bNA��A��9A���A��9A�=qA��A���A��A��A�7LA���A�-A�{A��A�5?A�ƨA� �A���A��A���A��-A�A�-A��TA�5?A��9A�O�A��A��^A�l�A���A��A��`A���A��mA�A��7A�bA���A��A���A��DA��+A��A�Q�A���A�%A�  A��PA���A��yA��-A���A�G�A�
=A~�/A};dA|�A{%AzM�Ay�wAxZAw��Av�As��Ap9XAm|�Ak�AiVAg7LAd�`A^ȴA];dA\��A\~�A[��A[p�AW�AU�^AS��AR~�AQ�-AP5?AMl�AL �AJ9XAHjAG��AG33AE��AD�!AD{AB�yAAx�A@��A?�-A>VA=A=hsA=G�A<�DA<A;��A:��A9�PA9XA8r�A7x�A65?A5p�A4{A3��A2��A1�;A1�A/��A/VA.��A-��A,JA+�hA*VA)�PA(�A&��A%%A$1A"��A!��A�-A�^Av�A�RA�A{A�wA��A�PA�
A1A��Ax�AhsAK�A/A��A/A��AffA  A�hA\)A
^5A	�PA	dZA	33A��AA�A$�A�wA�A%AdZAĜAn�A�FA�A�RA�A�#A ��A @�33@�V@��@�$�@���@�j@��;@�E�@�V@�ƨ@�M�@��/@�9@��@��H@�dZ@�n�@���@�9X@�P@��@���@�O�@���@�p�@�j@�\)@ى7@���@���@���@��@�G�@�1@ҟ�@�M�@��@��@�
=@�-@͑h@̣�@�|�@ɺ^@�\)@�@�ȴ@Ƈ+@Ƈ+@�t�@ŉ7@î@��@�-@���@�I�@��;@��@���@��H@���@§�@��^@�Ĝ@��w@�dZ@��@��/@�
=@�n�@��@��h@�Ĝ@�r�@�ƨ@�|�@�"�@��!@�$�@�@�O�@��/@��@�
=@���@��\@�E�@��#@���@�&�@���@��j@�Z@�dZ@��@�E�@��^@�V@���@���@�z�@��D@��
@���@��@��@�E�@���@���@�?}@��@�1'@��@��F@�\)@��!@�~�@�n�@�=q@�5?@�@�G�@���@��@��9@�(�@���@���@���@�t�@�+@�o@���@�E�@�p�@��@��@�A�@��@�;d@���@���@�V@�J@��T@���@��^@�/@��@���@���@�r�@�9X@� �@�  @��F@��@�C�@�@��H@�ȴ@���@���@��+@�M�@���@��T@��^@�hs@�/@���@���@���@�j@�Z@�Q�@�9X@� �@��@�1@��w@�|�@�|�@�K�@���@�^5@�$�@�{@��@���@��-@�p�@�?}@���@��u@�z�@�Z@��@���@��@��H@���@�ȴ@��R@��\@�ff@�E�@�-@�$�@��@��T@��#@���@�@��-@���@�hs@�/@��@�%@���@�Ĝ@���@�Q�@�(�@��@�b@�1@�1@�1@�1@���@��@��m@�ƨ@��@��@���@�t�@�K�@�"�@��H@��!@�ff@�=q@�-@�{@�{@���@�G�@��@��u@�(�@��@��
@���@��X@��Z@�S�@xPH@o��@hy>@_�}@V�1@N)�@E;@>M�@7E9@1�~@-V@*�@#�A@�@�1@�H@j@�9111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�^5A�^5A�^5A�^5A�`BA�`BA�bNA�`BA�`BA�`BA�^5A�bNA�^5A�E�Aƴ9A�ƨA�XA���A��hA�I�A�+A�ȴA�C�A��A��!A���A���A���A�hsA��A��A�A��PA�ffA�"�A���A��+A�-A�"�A���A��+A�Q�A�oA��/A�ƨA���A�7LA��A�VA�(�A��A��A���A�/A��A�1A���A��jA�/A��-A�bNA��A��9A���A��9A�=qA��A���A��A��A�7LA���A�-A�{A��A�5?A�ƨA� �A���A��A���A��-A�A�-A��TA�5?A��9A�O�A��A��^A�l�A���A��A��`A���A��mA�A��7A�bA���A��A���A��DA��+A��A�Q�A���A�%A�  A��PA���A��yA��-A���A�G�A�
=A~�/A};dA|�A{%AzM�Ay�wAxZAw��Av�As��Ap9XAm|�Ak�AiVAg7LAd�`A^ȴA];dA\��A\~�A[��A[p�AW�AU�^AS��AR~�AQ�-AP5?AMl�AL �AJ9XAHjAG��AG33AE��AD�!AD{AB�yAAx�A@��A?�-A>VA=A=hsA=G�A<�DA<A;��A:��A9�PA9XA8r�A7x�A65?A5p�A4{A3��A2��A1�;A1�A/��A/VA.��A-��A,JA+�hA*VA)�PA(�A&��A%%A$1A"��A!��A�-A�^Av�A�RA�A{A�wA��A�PA�
A1A��Ax�AhsAK�A/A��A/A��AffA  A�hA\)A
^5A	�PA	dZA	33A��AA�A$�A�wA�A%AdZAĜAn�A�FA�A�RA�A�#A ��A @�33@�V@��@�$�@���@�j@��;@�E�@�V@�ƨ@�M�@��/@�9@��@��H@�dZ@�n�@���@�9X@�P@��@���@�O�@���@�p�@�j@�\)@ى7@���@���@���@��@�G�@�1@ҟ�@�M�@��@��@�
=@�-@͑h@̣�@�|�@ɺ^@�\)@�@�ȴ@Ƈ+@Ƈ+@�t�@ŉ7@î@��@�-@���@�I�@��;@��@���@��H@���@§�@��^@�Ĝ@��w@�dZ@��@��/@�
=@�n�@��@��h@�Ĝ@�r�@�ƨ@�|�@�"�@��!@�$�@�@�O�@��/@��@�
=@���@��\@�E�@��#@���@�&�@���@��j@�Z@�dZ@��@�E�@��^@�V@���@���@�z�@��D@��
@���@��@��@�E�@���@���@�?}@��@�1'@��@��F@�\)@��!@�~�@�n�@�=q@�5?@�@�G�@���@��@��9@�(�@���@���@���@�t�@�+@�o@���@�E�@�p�@��@��@�A�@��@�;d@���@���@�V@�J@��T@���@��^@�/@��@���@���@�r�@�9X@� �@�  @��F@��@�C�@�@��H@�ȴ@���@���@��+@�M�@���@��T@��^@�hs@�/@���@���@���@�j@�Z@�Q�@�9X@� �@��@�1@��w@�|�@�|�@�K�@���@�^5@�$�@�{@��@���@��-@�p�@�?}@���@��u@�z�@�Z@��@���@��@��H@���@�ȴ@��R@��\@�ff@�E�@�-@�$�@��@��T@��#@���@�@��-@���@�hs@�/@��@�%@���@�Ĝ@���@�Q�@�(�@��@�b@�1@�1@�1@�1@���@��@��m@�ƨ@��@��@���@�t�@�K�@�"�@��H@��!@�ff@�=q@�-@�{@�{@���@�G�@��@��u@�(�@��@��
G�O�@��X@��Z@�S�@xPH@o��@hy>@_�}@V�1@N)�@E;@>M�@7E9@1�~@-V@*�@#�A@�@�1@�H@j@�9111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B�B�B�B�B�B�B�B�B�B�B�B �B;dB&�B9XBQ�Bm�Bt�Bu�Bz�Bv�Bv�B|�B�7B�\B��B��B��B��B��B��B��B��B��B�B�'B�'B�3B�XB�dB�wB��B��B��BÖBŢBǮBȴBǮBǮBǮBɺBȴBɺBȴBǮBǮBŢBÖB�}B�FB�B��B��B��B�=B�By�Bt�Bp�Bl�Be`BaHB^5BXBM�BC�B(�B�BbBB��B�HBÖB��B��B��B�JB�1B~�Bn�BgmBW
BA�B49B0!B,B+B#�B�B�B
��B
��B
�B
�ZB
��B
��B
�XB
�!B
��B
�DB
n�B
hsB
dZB
[#B
O�B
H�B
C�B
A�B
A�B
=qB
:^B
1'B
"�B

=B	��B	�B	�NB	�B	��B	��B	��B	��B	��B	��B	�bB	~�B	t�B	k�B	ffB	`BB	VB	H�B	A�B	8RB	0!B	/B	,B	#�B	�B	�B	�B	DB	
=B	%B��B��B��B��B��B��B�B�B�sB�fB�NB�/B�B��B��B��BǮBĜB��B�jB�XB�FB�-B�B�B��B��B��B��B��B�hB�PB�7B�B� B{�Bw�Bt�Br�Bq�Bm�Be`BXBT�BT�BS�BS�BR�BQ�BO�BO�BN�BM�BM�BL�BJ�BK�BK�BJ�BJ�BJ�BI�BI�BI�BI�BI�BK�BK�BL�BK�BJ�BJ�BI�BH�BI�BI�BI�BH�BH�BG�BH�BH�BH�BH�BL�BK�BL�BM�BM�BM�BN�BT�BYBXBVBT�BVBZB]/BZB[#B]/B_;B_;B^5B_;B_;B`BBbNBe`BhsBm�Bp�Br�Bt�Bu�Bu�Bv�By�B}�B�B�B�+B�PB�hB��B��B��B��B��B��B��B��B��B�3B�qBȴB��BɺBƨBȴB��B��B��B��B�BB�ZB�fB�yB�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B	B	B	+B	
=B	JB	hB	uB	�B	�B	�B	�B	%�B	&�B	(�B	1'B	5?B	6FB	8RB	:^B	>wB	A�B	B�B	E�B	G�B	H�B	H�B	I�B	J�B	M�B	M�B	M�B	N�B	O�B	T�B	W
B	W
B	W
B	ZB	]/B	]/B	_;B	cTB	iyB	m�B	o�B	r�B	w�B	z�B	}�B	� B	�B	�B	�B	�B	�B	�+B	�PB	�PB	�PB	�\B	�hB	�hB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�!B	�!B	�-B	�9B	�9B	�FB	�XB	�dB	�jB	�qB	�}B	�}B	��B	��B	B	ĜB	ƨB	ƨB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�
B	�
B	�B	�B	�B	�B	�#B	�5B	�BB	�NB	�TB	�TB	�ZB	�ZB	�ZB	�ZB	�`B	�fB	�fB	�fB	�mB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	�B
�B
7B
(sB
0oB
:�B
@ B
CaB
H�B
N"B
TaB
W�B
]~B
^�B
d�B
l�B
p!B
t�B
v�B
|�B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B�B�B�B�B�B�B�B�B�B�B�B	�B	�B!B*�B)B(�BA+B\�Bc�Bd�BjBe�Be�Bl!BxlB~�B��B��B��B��B��B�B�B�B�#B�7B�[B�[B�eB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�HB�1B�B��ByxBqIBiBc�B_�B[�BT�BP�BMqBGSB=B2�B:B
�B��B�jB�&BКB��B�+B�B��B{�Bw�Bn\B]�BV�BFpB0�B#�B�BrBmBFB"B�B
�rB
�EB
�B
��B
�VB
�B
��B
��B
�@B
z�B
^ B
W�B
S�B
J�B
?nB
8CB
3&B
1B
1B
-B
)�B
 �B
iB	��B	�wB	�9B	��B	ŦB	�-B	��B	�YB	�RB	�HB	�4B	�B	n�B	dmB	[9B	VB	O�B	E�B	8nB	1AB	(B	�B	�B	�B	�B	uB		_B	FB�B�B��B�B��B��B�B�B�B�yB�YB�=B�.B�B��B��B��B��B��B�zB�jB�TB�7B�%B�B� B��B��B��B��B��B�oB�WB�?B}(ByBs�Bo�Bk�Bg�Bd�Bb�Ba�B]qBU>BG�BD�BD�BC�BC�BB�BA�B?�B?�B>�B=�B=�B<�B:�B;�B;�B:�B:�B:�B9�B9�B9�B9�B9�B;�B;�B<�B;�B:�B:�B9�B8�B9�B9�B9�B8�B8�B7�B8�B8�B8�B8�B<�B;�B<�B=�B=�B=�B>�BD�BI BG�BE�BD�BE�BJBMBJBKBMBO%BO&BN!BO&BO&BP.BR9BUJBX]B]yB`�Bb�Bd�Be�Be�Bf�Bi�Bm�Bq�Br�BwB}7B�QB��B��B��B�xB�rB��B��B��B��B�B�TB��B��B��B��B��B��B��B��B��B� B�<B�DB�VB�_B�fB�eB�gB�vB�wB��B�B�B�B�B�B�B�B�B��B��B��B��B��B��B�B�B�#B	AB	QB	YB	^B	}B	�B	�B	�B	�B	! B	%B	&B	((B	*2B	.NB	1\B	2bB	5xB	7�B	8�B	8�B	9�B	:�B	=�B	=�B	=�B	>�B	?�B	D�B	F�B	F�B	F�B	I�B	L�B	MB	OB	S$B	YKB	]aB	_mB	b�B	g�B	j�B	m�B	o�B	q�B	t�B	t�B	t�B	t�B	v�B	}B	}B	}!B	'B	�4B	�5B	�9B	�MB	�QB	�cB	�uB	�yB	�}B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�B	�"B	�,B	�1B	�;B	�EB	�CB	�LB	�OB	�ZB	�eB	�qB	�qB	�wB	��B	��B	��B	��B	��B	��B	��B	��B	��B	¶B	üB	þB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�%B	�*B	�*B	�)B	�*B	�@B	�>B	�>B	�BB	�JB	�NB	�VB	�[B	�aB	�eB	�nB	�mB	�mB	�rB	�B	�B	�}B	�B	�B	�G�O�B	��B	��B
	�B
2B
 +B
*jB
/�B
3B
8sB
=�B
DB
GKB
M9B
N�B
TKB
\bB
_�B
dCB
fmB
l>B
p�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.11 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9996(+/-0.0001), vertically averaged dS =-0.016(+/-0.003) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Map scales: x=6,3; y=2,1. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                            201906040941462019060409414620190604094146  AO  ARCAADJP                                                                    20180524220042    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20180524220042  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20180524220042  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604094146  IP                  G�O�G�O�G�O�                