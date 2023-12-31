CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-07-29T09:16:12Z AOML 3.0 creation; 2016-08-07T21:17:39Z UW 3.1 conversion     
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
_FillValue                 �  A|   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  Ct   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  KP   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  MH   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U$   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]    TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  x�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  z|   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �X   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �P   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �,   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �\   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �\   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �\   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �\   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �    HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20150729091612  20160807141739  5904460 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               @A   AO  5285_8895_064                   2C  D   APEX                            6487                            072314                          846 @�c�Z���1   @�c��$@*g-�c���vȴ1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    @A   B   B   @�ff@�  A   A   A@  A`  A~ffA�  A�  A���A���A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C�C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� DtٚDyy�D�3D�FfD�y�D��fD�3D�<�D�p D��fD���D�<�D�� D��3D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��
@�p�A
�RA*�RAJ�RAj�RA��\A�\)A�\)A�(�A�(�A�\)A�\)A�\)B�B
�B�B�B"�B*�B2�B:�BB�BJ�BR�BZ�Bb�Bj�Br�Bz�B�W
B�W
B��=B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B��B�#�B�W
B�W
B�W
B�W
B�W
B�W
B�W
Bъ=B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
C ��C�C��C��C��C
��C��C��C��C��C��C��C��C��C��C��C ��C"��C$��C&��C(��C*��C,��C.��C0��C2��C4��C6��C8��C:��C<��C>��C@��CB��CD��CF��CH��CJ��CL��CN��CP��CR��CT��CV��CX��CZ��C\��C^��C`��Cb��Cd��Cf��Ch��Cj��Cl��Cn��Cp��Cr��Ct��Cv��Cx��Cz��C|��C~��C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�D *�D ��D*�D��D*�D��D*�D��D*�D��D*�D��D*�D��D*�D��D*�D��D	*�D	��D
*�D
��D*�D��D*�D��D*�D��D*�D��D*�D��D*�D��D*�D��D*�D��D*�D��D*�D��D*�D��D*�D��D*�D��D*�D��D*�D��D*�D��D*�D��D*�D��D*�D��D*�D��D*�D��D *�D ��D!*�D!��D"*�D"��D#*�D#��D$*�D$��D%*�D%��D&*�D&��D'*�D'��D(*�D(��D)*�D)��D**�D*��D+*�D+��D,*�D,��D-*�D-��D.*�D.��D/*�D/��D0*�D0��D1*�D1��D2*�D2��D3*�D3��D4*�D4��D5*�D5��D6*�D6��D7*�D7��D8*�D8��D9*�D9��D:*�D:��D;*�D;��D<*�D<��D=*�D=��D>*�D>��D?*�D?��D@*�D@��DA*�DA��DB*�DB��DC*�DC��DD*�DD��DE*�DE��DF*�DF��DG*�DG��DH*�DH��DI*�DI��DJ*�DJ��DK*�DK��DL*�DL��DM*�DM��DN*�DN��DO*�DO��DP*�DP��DQ*�DQ��DR*�DR��DS*�DS��DT*�DT��DU*�DU��DV*�DV��DW*�DW��DX*�DX��DY*�DY��DZ*�DZ��D[*�D[��D\*�D\��D]*�D]��D^*�D^��D_*�D_��D`*�D`��Da*�Da��Db*�Db��Dc*�Dc��Dd*�Dd��De*�De��Df*�Df��Dg*�Dg��Dh*�Dh��Di*�Di��Dj*�Dj��Dk*�Dk��Dl*�Dl��Dm*�Dm��Dn*�Dn��Do*�Do��Dp*�Dp��Dq*�Dq��Dr*�Dr��Ds*�Ds��Dt*�Dt��Du{Dy�{D��D�[�D��D���D�(�D�R>D��qD���D�>D�R>D��qD�ؤD��q11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A���A�wA�^A�!AᛦA�r�A�Q�A�=qA�5?A�-A� �A��A��A��A��A�oA�bA�bA�VA�JA�
=A�1A�A�  A��A�jA߰!A���AۮA�"�A�ƨA�ZA���A�hsA�A�Aџ�A�E�A��A�G�A�ffA�33A��`A��
A�9XA�"�A�1Aǟ�AƸRA���A�=qA�bNA�XA�JA�VA��A�ZA�O�A�33A��A�l�A�I�A��DA�|�A��`A�z�A���A�JA��#A���A��A�I�A��-A�I�A��uA�A�~�A� �A�jA�&�A�
=A�oA��A�&�A�t�A���A{�At�Ai�TAdI�A^=qAU��AO�AKG�AIS�AG"�AE��AC��AB��A@��A>�`A<��A9�A7�PA3p�A1ƨA2A�A1�
A/�A-�A+�wA*5?A'�wA&VA%�A$=qA"�!A!�#A bNA�A��A��AVAr�A{AJA�A�AjA��A��A�mAoA9XA�yA�!A��AZAAl�AĜAbNA1'A��A?}A�yA�A1'A�
A
=A��A�A��Ap�A33A��A1'Ax�A7LA+AȴA{AXA%AQ�A��A�!A^5A=qAr�AA�A�A1A��AXAoA
5?A	��A	�FA	�A	|�A	7LA�A-AS�A��A1At�AA��A�A��A�^AS�A�9A^5A�
A��A��A33A �HA v�A 5?@�S�@���@�J@�p�@�bN@�S�@�@���@��@��^@��@��@���@�o@�{@�hs@��@�z�@�(�@�"�@�V@�^@�9@�b@��m@�F@��H@�7@�&�@��@��@�z�@�1'@��@���@��@���@�1'@�P@�+@�^5@�^5@��@�-@䛦@�b@�@�ȴ@�^@���@�@��D@�r�@�b@߅@�\)@��@��H@���@ް!@ݙ�@���@�r�@��@�ƨ@�33@ڧ�@ٺ^@�?}@�bN@�ƨ@�33@���@�V@�{@��@�j@ӶF@�o@ҧ�@��@�7L@��/@�I�@ύP@��@ΰ!@�E�@��@͉7@�/@˕�@�;d@�33@ʟ�@�@���@�x�@��`@�A�@ǶF@�
=@��@ƸR@Ə\@�V@�-@��@�`B@�Ĝ@ċD@�I�@��m@Å@�o@\@�V@��-@�hs@�O�@��`@��D@�I�@��
@��@�K�@��R@�E�@���@�G�@���@�Z@�9X@�b@�l�@��@�ȴ@�~�@�-@��@�%@�Ĝ@��@��w@�|�@�\)@���@�$�@��#@�p�@���@�9X@���@���@�;d@�ff@��@���@�7L@��j@�Z@�1@���@�33@�@��@��+@�^5@�E�@�-@�@��h@�G�@��@���@�Z@�1'@�ƨ@�33@��y@���@���@�v�@�v�@�^5@�5?@��7@�?}@��j@�9X@���@��w@���@�
=@�ȴ@��\@��@�hs@�%@��/@�Q�@��F@�t�@�
=@��@�ȴ@���@��\@�ff@�M�@��@���@�G�@���@�Ĝ@��@�1@���@��@��P@�\)@�"�@���@��\@�=q@�@���@��^@��7@�`B@�O�@�/@��@���@���@�z�@�A�@�1@�ƨ@��@�o@��@��!@�V@�$�@�@��@��T@���@�?}@���@��j@��@���@�Z@�b@���@�\)@�C�@��y@���@�v�@�ff@�5?@�J@�@��@�p�@�7L@�&�@�V@���@��9@�r�@�b@��
@���@�l�@�o@�ȴ@�M�@�{@��^@�X@�V@��9@���@��H@\)@v{@l�@d�@[33@SC�@K�@Fff@>@6$�@/�@(  11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111 A���A���A�wA�^A�!AᛦA�r�A�Q�A�=qA�5?A�-A� �A��A��A��A��A�oA�bA�bA�VA�JA�
=A�1A�A�  A��A�jA߰!A���AۮA�"�A�ƨA�ZA���A�hsA�A�Aџ�A�E�A��A�G�A�ffA�33A��`A��
A�9XA�"�A�1Aǟ�AƸRA���A�=qA�bNA�XA�JA�VA��A�ZA�O�A�33A��A�l�A�I�A��DA�|�A��`A�z�A���A�JA��#A���A��A�I�A��-A�I�A��uA�A�~�A� �A�jA�&�A�
=A�oA��A�&�A�t�A���A{�At�Ai�TAdI�A^=qAU��AO�AKG�AIS�AG"�AE��AC��AB��A@��A>�`A<��A9�A7�PA3p�A1ƨA2A�A1�
A/�A-�A+�wA*5?A'�wA&VA%�A$=qA"�!A!�#A bNA�A��A��AVAr�A{AJA�A�AjA��A��A�mAoA9XA�yA�!A��AZAAl�AĜAbNA1'A��A?}A�yA�A1'A�
A
=A��A�A��Ap�A33A��A1'Ax�A7LA+AȴA{AXA%AQ�A��A�!A^5A=qAr�AA�A�A1A��AXAoA
5?A	��A	�FA	�A	|�A	7LA�A-AS�A��A1At�AA��A�A��A�^AS�A�9A^5A�
A��A��A33A �HA v�A 5?@�S�@���@�J@�p�@�bN@�S�@�@���@��@��^@��@��@���@�o@�{@�hs@��@�z�@�(�@�"�@�V@�^@�9@�b@��m@�F@��H@�7@�&�@��@��@�z�@�1'@��@���@��@���@�1'@�P@�+@�^5@�^5@��@�-@䛦@�b@�@�ȴ@�^@���@�@��D@�r�@�b@߅@�\)@��@��H@���@ް!@ݙ�@���@�r�@��@�ƨ@�33@ڧ�@ٺ^@�?}@�bN@�ƨ@�33@���@�V@�{@��@�j@ӶF@�o@ҧ�@��@�7L@��/@�I�@ύP@��@ΰ!@�E�@��@͉7@�/@˕�@�;d@�33@ʟ�@�@���@�x�@��`@�A�@ǶF@�
=@��@ƸR@Ə\@�V@�-@��@�`B@�Ĝ@ċD@�I�@��m@Å@�o@\@�V@��-@�hs@�O�@��`@��D@�I�@��
@��@�K�@��R@�E�@���@�G�@���@�Z@�9X@�b@�l�@��@�ȴ@�~�@�-@��@�%@�Ĝ@��@��w@�|�@�\)@���@�$�@��#@�p�@���@�9X@���@���@�;d@�ff@��@���@�7L@��j@�Z@�1@���@�33@�@��@��+@�^5@�E�@�-@�@��h@�G�@��@���@�Z@�1'@�ƨ@�33@��y@���@���@�v�@�v�@�^5@�5?@��7@�?}@��j@�9X@���@��w@���@�
=@�ȴ@��\@��@�hs@�%@��/@�Q�@��F@�t�@�
=@��@�ȴ@���@��\@�ff@�M�@��@���@�G�@���@�Ĝ@��@�1@���@��@��P@�\)@�"�@���@��\@�=q@�@���@��^@��7@�`B@�O�@�/@��@���@���@�z�@�A�@�1@�ƨ@��@�o@��@��!@�V@�$�@�@��@��T@���@�?}@���@��j@��@���@�Z@�b@���@�\)@�C�@��y@���@�v�@�ff@�5?@�J@�@��@�p�@�7L@�&�@�V@���@��9@�r�@�b@��
@���@�l�@�o@�ȴ@�M�@�{@��^@�X@�VG�O�@���@��H@\)@v{@l�@d�@[33@SC�@K�@Fff@>@6$�@/�@(  11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	{�B	{�B	z�B	z�B	z�B	y�B	y�B	y�B	y�B	y�B	y�B	x�B	x�B	x�B	x�B	x�B	x�B	x�B	x�B	x�B	x�B	x�B	w�B	x�B	x�B	w�B	v�B	t�B	{�B	�+B	�=B	�7B	s�B	�%B	��B	��B	�wB	��B	�;B
B
5?B
� B6FB|�B��B�/B
=B.BL�BH�BiyBn�BZB�7B��B��B�FB�!B�B��B�BVBF�BF�BF�B
=B��B�BɺB�XB�!B�?B��B��By�BffBC�B(�B�BB
��B
�B
��B
p�B
6FB	��B	�wB	�bB	R�B	5?B	�B�B�B��B��B��B�B�B�B�BB�B��B	{B	�B	oB	DB	+B	=qB	9XB	@�B	9XB	49B	,B	)�B	-B	'�B	)�B	0!B	1'B	>wB	E�B	YB	hsB	q�B	�1B	��B	��B	��B	�}B	ĜB	ǮB	�
B	��B	ȴB	��B	��B	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	��B	��B	��B	��B	�)B	�HB	�NB	�fB	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B
B
B
B
B
B
B	��B	��B
B
B
B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
	7B
DB
DB
JB
PB
PB
PB
PB
JB
JB
JB
PB
PB
VB
VB
VB
VB
VB
VB
VB
VB
VB
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
hB
hB
bB
bB
bB
bB
hB
hB
hB
hB
hB
hB
hB
oB
oB
oB
oB
oB
oB
oB
uB
uB
uB
uB
uB
uB
uB
uB
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
%�B
%�B
/B
2-B
9XB
?}B
F�B
N�B
S�B
XB
ZB
_;B
dZB
hsB
m�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111 B	{�B	{�B	z�B	z�B	z�B	y�B	y�B	y�B	y�B	y�B	y�B	x�B	x�B	x�B	x�B	x�B	x�B	x�B	x�B	x�B	x�B	x�B	w�B	x�B	x�B	w�B	v�B	t�B	{�B	�B	�B	�B	s�B	��B	�eB	��B	�PB	ͩB	�B
�B
5B
�B6B|�B��B��B
B-�BL�BH{Bi?Bn^BY�B��B�kB��B�B��B��B�iB��BU�BFlBFiBFjB	�B��B��B�|B�B��B� B��B�GBy�Bf&BCUB(�BUB �B
ˇB
��B
�IB
pfB
6
B	��B	�?B	�*B	R�B	5B	PB�nB��B��BһB��B��B��B��B�	B�EB��B	@B	}B	2B		B	*�B	=2B	9B	@DB	9B	3�B	+�B	)�B	,�B	'�B	)�B	/�B	0�B	>5B	EaB	X�B	h4B	qfB	��B	�CB	�UB	��B	�8B	�UB	�gB	��B	ϕB	�lB	�EB	�?B	�nB	ˁB	͋B	ϗB	ΒB	ѤB	ӰB	ԸB	ԷB	պB	��B	��B	ջB	ԵB	ӰB	ӱB	ӱB	��B	��B	�B	�B	�IB	�^B	�xB	�yB	�zB	�rB	��B	��B	��B	�~B	�kB	�mB	��B
 �B
 �B
 �B
 �B
 �B
 �B	��B	��B
�B
�B
 �B	��B	��B	��B	�cB	�cB	�cB	�YB	�aB	�dB	�`B	�_B	�rB	�pB	�fB	�jB	�qB	�}B	�|B	��B	��B	��B	��B	��B	�~B	�~B	�|B	�vB	�pB	�sB	�|B	�vB	�qB	�jB	�lB	�bB	�`B	�ZB	�SB	�YB	�YB	�VB	�QB	�eB	�kB	�iB	�iB	�eB	�_B	�YB	�_B	�^B	�`B	�\B	�]B	�XB	�PB	�XB	�XB	�KB	�@B	�9B	�1B	�:B	�EB	�LB	�WB	�LB	�QB	�JB	�FB	�>B	�?B	�CB	�CB	�SB	�^B	�`B	�cB	�cB	�cB	�bB	�^B	�`B	�hB	�iB	�iB	�bB	�iB	�cB	�cB	�aB	�]B	�]B	�WB	�VB	�WB	�VB	�XB	�PB	�CB	�EB	�CB	�>B	�>B	�?B	�>B	�?B	�DB	�EB	�AB	�OB	�UB	�VB	�PB	�OB	�PB	�IB	�OB	�PB	�UB	�MB	�VB	�WB	�\B	�^B	�]B	�\B	�\B	�\B	�]B	�aB	�gB	�hB	�gB	�nB	�lB	�uB	�rB	�tB	�sB	�mB	�pB	�mB	�nB	�uB	�tB	�uB	�vB	�tB	�sB	�|B	�{B	��B	�~B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B

�B

�B
�B
�B
B
�B
B
�B
�B
�B
�B
 B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
#B
&B
$B
$B
$B
$B
#B
$B
)B
)B
(B
0B
0B
1B
1B
.B
/B
6B
<B
<B
<B
;B
<B
:B
CB
DB
CB
IB
HB
IB
GB
IB
IB
VB
\B
`B
dB
cB
cB
fB
lB
jB
lB
lB
 tB
 uB
 sB
 uB
 sB
 rB
 vB
!xB
!xG�O�B
%�B
.�B
1�B
9B
?*B
FTB
N�B
S�B
W�B
Y�B
^�B
dB
hB
m=11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.67 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071417392016080714173920160807141739  AO  ARCAADJP                                                                    20150729091612    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150729091612  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20150729091612  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807141739  IP                  G�O�G�O�G�O�                