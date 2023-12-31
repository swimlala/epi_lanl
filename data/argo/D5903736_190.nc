CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2017-07-21T00:00:53Z creation      
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
_FillValue                    �,Argo profile    3.1 1.2 19500101000000  20170721000053  20190604094028  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4051                            2C  D   APEX                            5368                            041511                          846 @�5ϥ(1   @�6b��D@5�$�/�d�V�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8ffB@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De�fDf  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dy��D��D�.D�w\D���D�D�Z=D���D���D���D�G\D�{3D��HD��\D�O
Dڒ=D��HD�
D�>�D�w\11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @x��@�z�@�z�A=qA>=qA^=qA~=qA��A��A��A��A��A��A��A��B�\B�\B�\B�\B'�\B/�\B7��B?�\BG�\BO�\BW�\B_�\Bg�\Bo�\Bw�\B�\B�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB���B�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮC��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D x�D ��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��D	x�D	��D
x�D
��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��D x�D ��D!x�D!��D"x�D"��D#x�D#��D$x�D$��D%x�D%��D&x�D&��D'x�D'��D(x�D(��D)x�D)��D*x�D*��D+x�D+��D,x�D,��D-x�D-��D.x�D.��D/x�D/��D0x�D0��D1x�D1��D2x�D2��D3x�D3��D4x�D4��D5x�D5��D6x�D6��D7x�D7��D8x�D8��D9x�D9��D:x�D:��D;x�D;��D<x�D<��D=x�D=��D>x�D>��D?x�D?��D@x�D@��DAx�DA��DBx�DB��DCx�DC��DDx�DD��DEx�DE��DFx�DF��DGx�DG��DHx�DH��DIx�DI��DJx�DJ��DKx�DK��DLx�DL��DMx�DM��DNx�DN��DOx�DO��DPx�DP��DQx�DQ��DRx�DR��DSx�DS��DTx�DT��DUx�DU��DVx�DV��DWx�DW��DXx�DX��DYx�DY��DZx�DZ��D[x�D[��D\x�D\��D]x�D]��D^x�D^��D_x�D_��D`x�D`��Dax�Da��Dbx�Db��Dcx�Dc��Ddx�Dd��De\De��Dfx�Df��Dgx�Dg��Dhx�Dh��Dix�Di��Djx�Dj��Dkx�Dk��Dlx�Dl��Dmx�Dm��Dnx�Dn��Dox�Do��Dpx�Dp��Dqx�Dq��Drx�Dr��Dsx�Ds��Dtx�Dt��Dy|�D� RD�*�D�s�D��gD��D�V�D��\D��
D��qD�C�D�w�D���D���D�K�Dڎ�D���D��D�;3D�s�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��Aݙ�Aݗ�AݓuAݏ\Aݏ\Aݏ\A݉7A݋DA݇+A݅A݁A݃A݁A݁A�S�A�|�A�G�A�VA�bNAڑhAٮA�M�Aغ^A֝�AӴ9A�\)A��`A�jA̰!A�A�A˶FAʓuAɇ+A�ĜA�?}A��;A�S�A�-A�I�A�(�A�p�A�Q�A��A��`A�r�A���A�n�A�A��DA�JA���A���A�I�A���A�/A��A�dZA��A���A�S�A�S�A��A�E�A�O�A���A�A���A�=qA��+A���A��A��HA��RA��A���A��hA�r�A�5?A���A�oA���A�z�A�l�A�/A��mA���A�;dA�ƨA��PA��A�r�A�JA��/A���A�VA��/A�oA���A��A���A�ĜA�33A�VA��DA�l�A��;A�A�A�33A��wA�+A���A�v�A��A�x�A��A�l�A�^5A�I�A�7LA� �A��9A�|�A���A~�uA|��Ay�hAx�!Aw;dAt�`ArjApbNAm
=AkhsAg��Ad�\Ab��Aa��A`�yA_�-A^��A^z�A\�yA[K�AW�ASG�AQ��AP��ANVAL�ALJAK7LAH�AF�AE��AD�9ACK�AB��AB1'A@�!A>�A;��A:9XA9&�A7+A6 �A4��A2��A1/A0Q�A/�;A/��A/�A-+A+�A+��A+\)A*z�A)�A'��A&�uA%VA#��A"ĜA ��AXA-AoAQ�AK�A�+A��A��A��A
=AJA;dA�AG�A�A��A��AA�A33A
�DA	�AffA�A��A�mA�Ax�Az�A�wA �`A -A @�V@���@�Ĝ@�t�@��-@�bN@��y@��#@�9X@�K�@��H@�+@�@�@�1@�dZ@�\@���@��@�Q�@�w@�~�@�ƨ@�$�@�X@�S�@��@�G�@�&�@��@��@���@�Z@އ+@�;d@��@�Ĝ@ָR@�Ĝ@� �@�t�@�K�@���@�~�@���@ёh@��/@�Q�@���@�-@��@�1@�ƨ@ˮ@�t�@�~�@�x�@�1'@�E�@�x�@���@�z�@å�@���@�v�@���@��@�/@��@�9X@��@��@��D@�ƨ@�n�@�p�@�p�@�%@��@�dZ@�^5@��#@��-@�p�@���@��@��R@�E�@�{@�@�x�@�O�@��@��@�bN@�E�@���@�j@�1@��m@�K�@�o@���@��R@���@��+@�5?@���@���@�+@�;d@�K�@��@��+@���@�V@�Q�@�1@�b@��@���@��
@�33@�
=@���@�$�@���@�X@�z�@���@��
@��m@��w@��@�l�@���@�$�@��T@��-@�`B@�/@���@���@�z�@�A�@��m@��@�"�@�
=@��y@��\@�J@�p�@�&�@�%@��/@��@�j@�A�@���@�\)@�C�@���@�9X@� �@��D@���@��`@��@�p�@�x�@�p�@�`B@�/@���@�Q�@��@�r�@�9X@��@���@�dZ@�+@���@�V@�@���@�@�=q@�-@�J@��#@��^@��h@���@��9@�Z@��@��@���@�C�@��!@���@��\@�v�@��@�33@�;d@�K�@��@��@�ȴ@��!@��\@�~�@�ff@�M�@�=q@���@��^@��7@�X@���@��@���@�Q�@�(�@� �@��@�b@�b@�1@���@�|�@�\)@�
=@�ȴ@��!@��+@�^5@�-@�{@��T@�@�p�@�G�@�%@���@��9@��@���@��u@�bN@�  @��m@���@��@�ȴ@��@�p�@��@�Ĝ@��j@��9@��@��@���@��D@�Q�@���@�\)@��@���@�:�@y�3@q}�@e�@Zz@R~�@K�&@Ef�@>�1@8A�@36z@*��@$V�@�@�j@�@/@V@�M11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111   Aݙ�Aݗ�AݓuAݏ\Aݏ\Aݏ\A݉7A݋DA݇+A݅A݁A݃A݁A݁A�S�A�|�A�G�A�VA�bNAڑhAٮA�M�Aغ^A֝�AӴ9A�\)A��`A�jA̰!A�A�A˶FAʓuAɇ+A�ĜA�?}A��;A�S�A�-A�I�A�(�A�p�A�Q�A��A��`A�r�A���A�n�A�A��DA�JA���A���A�I�A���A�/A��A�dZA��A���A�S�A�S�A��A�E�A�O�A���A�A���A�=qA��+A���A��A��HA��RA��A���A��hA�r�A�5?A���A�oA���A�z�A�l�A�/A��mA���A�;dA�ƨA��PA��A�r�A�JA��/A���A�VA��/A�oA���A��A���A�ĜA�33A�VA��DA�l�A��;A�A�A�33A��wA�+A���A�v�A��A�x�A��A�l�A�^5A�I�A�7LA� �A��9A�|�A���A~�uA|��Ay�hAx�!Aw;dAt�`ArjApbNAm
=AkhsAg��Ad�\Ab��Aa��A`�yA_�-A^��A^z�A\�yA[K�AW�ASG�AQ��AP��ANVAL�ALJAK7LAH�AF�AE��AD�9ACK�AB��AB1'A@�!A>�A;��A:9XA9&�A7+A6 �A4��A2��A1/A0Q�A/�;A/��A/�A-+A+�A+��A+\)A*z�A)�A'��A&�uA%VA#��A"ĜA ��AXA-AoAQ�AK�A�+A��A��A��A
=AJA;dA�AG�A�A��A��AA�A33A
�DA	�AffA�A��A�mA�Ax�Az�A�wA �`A -A @�V@���@�Ĝ@�t�@��-@�bN@��y@��#@�9X@�K�@��H@�+@�@�@�1@�dZ@�\@���@��@�Q�@�w@�~�@�ƨ@�$�@�X@�S�@��@�G�@�&�@��@��@���@�Z@އ+@�;d@��@�Ĝ@ָR@�Ĝ@� �@�t�@�K�@���@�~�@���@ёh@��/@�Q�@���@�-@��@�1@�ƨ@ˮ@�t�@�~�@�x�@�1'@�E�@�x�@���@�z�@å�@���@�v�@���@��@�/@��@�9X@��@��@��D@�ƨ@�n�@�p�@�p�@�%@��@�dZ@�^5@��#@��-@�p�@���@��@��R@�E�@�{@�@�x�@�O�@��@��@�bN@�E�@���@�j@�1@��m@�K�@�o@���@��R@���@��+@�5?@���@���@�+@�;d@�K�@��@��+@���@�V@�Q�@�1@�b@��@���@��
@�33@�
=@���@�$�@���@�X@�z�@���@��
@��m@��w@��@�l�@���@�$�@��T@��-@�`B@�/@���@���@�z�@�A�@��m@��@�"�@�
=@��y@��\@�J@�p�@�&�@�%@��/@��@�j@�A�@���@�\)@�C�@���@�9X@� �@��D@���@��`@��@�p�@�x�@�p�@�`B@�/@���@�Q�@��@�r�@�9X@��@���@�dZ@�+@���@�V@�@���@�@�=q@�-@�J@��#@��^@��h@���@��9@�Z@��@��@���@�C�@��!@���@��\@�v�@��@�33@�;d@�K�@��@��@�ȴ@��!@��\@�~�@�ff@�M�@�=q@���@��^@��7@�X@���@��@���@�Q�@�(�@� �@��@�b@�b@�1@���@�|�@�\)@�
=@�ȴ@��!@��+@�^5@�-@�{@��T@�@�p�@�G�@�%@���@��9@��@���@��u@�bN@�  @��m@���@��@�ȴ@��@�p�@��@�Ĝ@��j@��9@��@��@���@��D@�Q�@���@�\)G�O�@���@�:�@y�3@q}�@e�@Zz@R~�@K�&@Ef�@>�1@8A�@36z@*��@$V�@�@�j@�@/@V@�M11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBuBoBuBuBuBoBoBoBuBuBuB{B{B�B=qB��B�!B��B��BBbB�B#�B-BG�BW
Bz�B�uB��B��B��B��B��B��B��B��B��B��B��B�DB�7B�JB�{B��B��B��B��B��B��B��B��B�B�B�B�LB��BBĜB��B�;B�BB�/B�/B�/B�B�B�
B��B��B��B��BǮBƨBƨBŢBŢBÖB��B�jB�!B�{B�\B�%Bw�Be`BdZBo�Bv�Bu�Bt�Bs�BgmBL�B5?B�B�#B��B��B��B�+B|�Br�B_;BL�BG�B2-B�B
=BB
��B
�B
�
B
�jB
�B
��B
��B
�uB
�oB
�bB
�\B
�JB
�%B
x�B
l�B
XB
K�B
5?B
-B
"�B
oB
B	�B	�;B	��B	�^B	��B	��B	��B	�oB	�=B	�B	�B	w�B	k�B	YB	D�B	;dB	7LB	,B	%�B	 �B	�B	uB	JB	+B	B	  B��B��B��B�B�NB�)B�B�B��B��B��BŢBÖB��B��B�qB�RB�?B�3B�'B�B��B��B��B��B��B�{B�bB�PB�DB�1B�%B�B�B� B|�Bx�Bt�Br�Bq�Bp�Bm�Bk�Bk�BjBhsBgmBe`BffBdZBaHB`BB]/B\)BZBZBYBXBVBT�BVBW
BT�BS�BT�BT�BVBVBXBYBYBYBYBYBZBZB[#B\)B]/B^5B^5B^5BaHBe`BdZBgmBiyBk�Bl�Bl�Bl�Bl�Bk�Bk�Bo�Bo�Bl�BhsBhsBhsBhsBiyBl�Bm�Bn�Bm�Bm�Bo�Bu�Bw�Bz�B{�B{�B{�B|�B�B�B�+B�\B�uB��B��B��B��B��B��B��B��B��B�B�B�9B�XB�XB�^B�qB�qB�jB�jB�wBÖB��B��B��B��B��B��B��B��B�B�#B�)B�)B�)B�#B�TB�B�B�B�B�B�B�B��B��B��B��B��B	B	DB	\B	oB	�B	�B	�B	%�B	&�B	)�B	,B	1'B	33B	5?B	6FB	6FB	5?B	6FB	:^B	:^B	:^B	>wB	A�B	B�B	C�B	D�B	E�B	F�B	F�B	G�B	H�B	K�B	L�B	N�B	Q�B	R�B	VB	XB	[#B	^5B	^5B	_;B	bNB	ffB	k�B	n�B	o�B	p�B	q�B	s�B	u�B	y�B	}�B	�B	�1B	�JB	�bB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�'B	�3B	�9B	�FB	�LB	�RB	�RB	�dB	�qB	�}B	��B	ÖB	ĜB	ŢB	ƨB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�/B	�BB	�NB	�TB	�`B	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
%B
%B
+B
1B
1B
	7B
	7B
	7B
	7B
	7B
	7B
	7B
	7B

=B

=B
DB
&B
�B
%B
,�B
6zB
?�B
AB
E�B
I�B
QB
S�B
X�B
aB
f�B
mwB
o5B
sMB
u�B
x�B
{11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111   B	�B�B	�B	�B	�B�B�B�B	�B	�B	�B
�B
�B�B3�B��B�CB��B�B�)B}B�B�B#)B=�BM$Bp�B��B��B��B��B��B��B��B��B��B��B��B��B�[BKB�aB��B��B��B��B��B��B��B��B��B�B�(B�3B�dB��B��B��B�
B�KB�UB�?B�AB�AB�.B�)B�B�B�B��B��B��B��B��B��B��B��B��B��B�9B��B�wB|>Bm�B[�BZuBe�Bl�Bk�Bj�Bi�B]�BB�B+bB�B�RB��B�'B��B}aBs!Bh�BUoBCB=�B(dB�B ~B
�BB
�B
��B
�PB
��B
�aB
�(B
��B
��B
��B
��B
��B
��B
|oB
o B
b�B
N\B
BB
+�B
#aB
"B
�B	�aB	�B	ՓB	�=B	��B	�KB	�B	��B	��B	��B	{�B	wgB	n/B	a�B	OzB	;B	1�B	-�B	"oB	RB	/B	B		�B	�B��B��B�nB�aB�QB�,B��BؽBҚBωB�vB�gB�XB�3B�B�	B��B��B��B��B��B��B��B��B�gB�XB�8B�&B�B��B��B��B��B~�B|�Bz�By�BvyBshBoRBk:Bi-Bh&Bg"BdBbBbB`�B^�B]�B[�B\�BZ�BW�BV�BS�BR�BP�BP�BO�BN�BL�BK�BL�BM�BK�BJ}BK�BK�BL�BL�BN�BO�BO�BO�BO�BO�BP�BP�BQ�BR�BS�BT�BT�BT�BW�B[�BZ�B]�B_�Bb	BcBcBcBcBb
Bb	Bf$Bf%BcB^�B^�B^�B^�B_�BcBdBeBdBdBf$BlIBnTBqdBrjBrlBrmBsrBw�Bz�B}�B��B��B�B�B�.B�6B�AB�NB�cB�jB�zB��B��B��B��B��B��B��B��B��B��B��B�B�@B�GB�HB�HB�PB�cB�dB�zBЛBѣBҧBҨBҧBѤB��B�B�B�B�B�$B�.B�4B�NB�SB�]B�mB�xB��B	�B	�B	�B	�B	.B	9B	_B	aB	 xB	"�B	'�B	)�B	+�B	,�B	,�B	+�B	,�B	0�B	0�B	0�B	4�B	8B	9B	:B	;B	<B	= B	=#B	>%B	?-B	B?B	CHB	ERB	HcB	ImB	L{B	N�B	Q�B	T�B	T�B	U�B	X�B	\�B	a�B	eB	fB	gB	h!B	j*B	l8B	pRB	tjB	y�B	~�B	��B	��B	��B	�B	�B	�.B	�fB	�jB	�jB	�jB	�lB	�jB	�vB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�"B	�$B	�3B	�BB	�KB	�VB	�VB	�WB	�[B	�`B	�rB	�|B	ӢB	ֱB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�$B	�#B	�#B	�#B	�$B	�$B	�+B	�<B	�5B	�CB	�KB	�IB	�IB	�SB	�TB	�SB	�[B	�^B	�fB	�dB	�hB	�pB	�qB	�rB	�zB	�yB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �G�O�B
	�B
VB
~B
#*B
,�B
6QB
7tB
;�B
?�B
G�B
J.B
OhB
WB
]:B
c�B
e�B
i�B
l.B
o>B
q�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.11 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0), vertically averaged dS =-0.009(+/-0.001) in PSS-78.                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Map scales: x=6,3; y=2,1. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                            201906040940282019060409402820190604094028  AO  ARCAADJP                                                                    20170721000053    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20170721000053  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20170721000053  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604094028  IP                  G�O�G�O�G�O�                