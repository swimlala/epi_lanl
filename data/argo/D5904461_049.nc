CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-05-18T19:20:36Z AOML 3.0 creation; 2016-08-07T21:36:35Z UW 3.1 conversion     
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
_FillValue                 �  At   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  Cl   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  K@   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M8   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP_ADJUSTED            
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
resolution        :�o     �  px   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  xL   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  zD   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �@   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �D   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �H   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �L   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �P   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20150518192036  20160807143635  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               1A   AO  5286_8897_049                   2C  D   APEX                            6531                            072314                          846 @�Q�l��1   @�Q�@@2����o�c��`A�71   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    1A   B   B   A�ffA�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV�CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dp��Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dys3D�3D�33D��fD���D��D�L�D��3D���D��D�@ D�&fD���D�	�D�33D�vfD��3D�fD�C3D�y�D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��
A�p�A�p�A�p�A�p�B �RB�RB�RB�RB �RB(�RB0�RB8�RB@�RBH�RBP�RBX�RB`�RBh�RBp�RBx�RB�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)C .C.C.C.C.C
.C.C.C.C.C.C.C.C.C.C.C .C".C$.C&.C(.C*.C,.C..C0.C2.C4.C6.C8.C:.C<.C>.C@.CB.CD.CF.CH.CJ.CL.CN.CP.CR.CT.CVG�CX.CZ.C\.C^.C`.Cb.Cd.Cf.Ch.Cj.Cl.Cn.Cp.Cr.Ct.Cv.Cx.Cz.C|.C~.C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��DqDq��Dr�Dr��Ds�Ds��Dt�Dt��Dt�RDy~�D��D�8�D��)D�D�]D�R�D���D�ҐD��D�E�D�,)D�ҐD�]D�8�D�|)D���D�)D�H�D�]D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��/A��;A��HA��mA��TA��`A��`A��`A��TA��`A��HA��;A��HA��yA��A��
A���A���A���AͮA͓uA͉7A�z�A�dZA�G�A�A�A�7LA�7LA�33A�(�A�1A��A��A��A��A��A��`A���A̼jA̼jA̾wA���A̾wA̶FA�hsA�(�A�oA�bA�bA�VA�%A���A���A��A���A˟�A�hsA�oA���A�=qA�ȴAɗ�A�t�A��A��A�|�A�`BA�(�AƁA�1'A��Aŝ�A�z�A�C�AčPA���A�E�A��FA�r�A���A��
A��FA��A�|�A��!A��HA�p�A�K�A��A��+A�A�$�A�r�A���A���A�-A���A��A�(�A���A���A��DA���A�ƨA���A�9XA��TA���A�t�A�-A�7LA��A�?}A��TA���A�S�A�VA��PA�dZA�VA���A��uA��Aw�;Au+Aq��Ah1AdĜAbr�A_33A^5?A]?}AYO�AU��AT��ASAO/AM�wAL��AHVAEl�AC��AB�jABE�AA��AA�A@=qA<�A:��A9�FA9C�A9"�A8z�A7VA5��A4��A3��A2��A2$�A17LA.�\A-�A,bA+hsA)��A'hsA&bNA%�;A#�-A"  A!�7A!/A �/A ��A �9A�A�`AM�A�^A;dA%A�/AI�A1'A�AoAn�A��A��A��A �A1'A5?A=qA��A�PAVAK�A�A��AVA��AjAA�hA
��A	��A�9A&�A�RAM�A�A�yAjA�TA"�A ff@�@�t�@���@�@��@�I�@��@���@��@��#@��/@�F@��@�-@�@��@�M�@�7L@�@�b@땁@�+@�$�@�@�!@�Z@���@�|�@�@�1'@�\)@��y@��#@��@�z�@ّh@׍P@�v�@� �@ҧ�@�x�@�bN@��@϶F@ύP@�|�@�V@���@�|�@�
=@�ȴ@�M�@�J@�@ɉ7@�O�@���@�j@�1'@� �@�b@�1@�  @�  @���@���@���@��m@�1@�@�v�@���@�33@�"�@�33@�C�@�33@��@��@�5?@�p�@ȴ9@� �@ǍP@�
=@�J@��@��@�hs@��@�Ĝ@ēu@�  @öF@�C�@�"�@�
=@��@§�@�v�@�E�@���@�V@��D@�9X@���@���@��^@�hs@��/@�ƨ@�o@��@���@���@�/@��@���@�G�@��
@���@��m@���@��!@���@�v�@�$�@��@��#@���@��@�Ĝ@��P@���@���@�ƨ@� �@��
@�Z@�9X@�Z@�Q�@�1@�o@��T@��#@��@��@�1'@��@�l�@�o@�^5@�{@��@��#@��^@��h@���@���@�9X@���@�S�@��@��@���@�5?@���@��@��`@���@�I�@��@�33@�@�ȴ@���@�ff@�{@��-@��7@�?}@��@��@�V@���@��/@��@��j@��@��@���@��@�9X@���@���@�t�@�
=@�^5@��@�J@��@��^@���@��h@��h@��@�hs@�G�@�G�@�/@�&�@�V@��@��9@�Q�@�  @��
@��
@��@�dZ@�t�@�S�@�;d@�+@��@�@�@���@��@��R@�=q@���@�p�@���@�j@�(�@���@��@���@��y@��@��@���@��-@���@���@��^@�hs@��@���@���@�A�@�b@�ƨ@��@�S�@�+@���@���@�n�@�ff@�=q@�S�@��@��@z�@o�w@fv�@]�@T1@N�R@G
=@>�y@9%@3t�@*��@%p�@ Ĝ@�H@��@�@?}@	�#111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A��/A��;A��HA��mA��TA��`A��`A��`A��TA��`A��HA��;A��HA��yA��A��
A���A���A���AͮA͓uA͉7A�z�A�dZA�G�A�A�A�7LA�7LA�33A�(�A�1A��A��A��A��A��A��`A���A̼jA̼jA̾wA���A̾wA̶FA�hsA�(�A�oA�bA�bA�VA�%A���A���A��A���A˟�A�hsA�oA���A�=qA�ȴAɗ�A�t�A��A��A�|�A�`BA�(�AƁA�1'A��Aŝ�A�z�A�C�AčPA���A�E�A��FA�r�A���A��
A��FA��A�|�A��!A��HA�p�A�K�A��A��+A�A�$�A�r�A���A���A�-A���A��A�(�A���A���A��DA���A�ƨA���A�9XA��TA���A�t�A�-A�7LA��A�?}A��TA���A�S�A�VA��PA�dZA�VA���A��uA��Aw�;Au+Aq��Ah1AdĜAbr�A_33A^5?A]?}AYO�AU��AT��ASAO/AM�wAL��AHVAEl�AC��AB�jABE�AA��AA�A@=qA<�A:��A9�FA9C�A9"�A8z�A7VA5��A4��A3��A2��A2$�A17LA.�\A-�A,bA+hsA)��A'hsA&bNA%�;A#�-A"  A!�7A!/A �/A ��A �9A�A�`AM�A�^A;dA%A�/AI�A1'A�AoAn�A��A��A��A �A1'A5?A=qA��A�PAVAK�A�A��AVA��AjAA�hA
��A	��A�9A&�A�RAM�A�A�yAjA�TA"�A ff@�@�t�@���@�@��@�I�@��@���@��@��#@��/@�F@��@�-@�@��@�M�@�7L@�@�b@땁@�+@�$�@�@�!@�Z@���@�|�@�@�1'@�\)@��y@��#@��@�z�@ّh@׍P@�v�@� �@ҧ�@�x�@�bN@��@϶F@ύP@�|�@�V@���@�|�@�
=@�ȴ@�M�@�J@�@ɉ7@�O�@���@�j@�1'@� �@�b@�1@�  @�  @���@���@���@��m@�1@�@�v�@���@�33@�"�@�33@�C�@�33@��@��@�5?@�p�@ȴ9@� �@ǍP@�
=@�J@��@��@�hs@��@�Ĝ@ēu@�  @öF@�C�@�"�@�
=@��@§�@�v�@�E�@���@�V@��D@�9X@���@���@��^@�hs@��/@�ƨ@�o@��@���@���@�/@��@���@�G�@��
@���@��m@���@��!@���@�v�@�$�@��@��#@���@��@�Ĝ@��P@���@���@�ƨ@� �@��
@�Z@�9X@�Z@�Q�@�1@�o@��T@��#@��@��@�1'@��@�l�@�o@�^5@�{@��@��#@��^@��h@���@���@�9X@���@�S�@��@��@���@�5?@���@��@��`@���@�I�@��@�33@�@�ȴ@���@�ff@�{@��-@��7@�?}@��@��@�V@���@��/@��@��j@��@��@���@��@�9X@���@���@�t�@�
=@�^5@��@�J@��@��^@���@��h@��h@��@�hs@�G�@�G�@�/@�&�@�V@��@��9@�Q�@�  @��
@��
@��@�dZ@�t�@�S�@�;d@�+@��@�@�@���@��@��R@�=q@���@�p�@���@�j@�(�@���@��@���@��y@��@��@���@��-@���@���@��^@�hs@��@���@���@�A�@�b@�ƨ@��@�S�@�+@���@���@�n�@�ffG�O�@�S�@��@��@z�@o�w@fv�@]�@T1@N�R@G
=@>�y@9%@3t�@*��@%p�@ Ĝ@�H@��@�@?}@	�#111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	�5B	�5B	�5B	�5B	�5B	�5B	�5B	�5B	�5B	�5B	�5B	�5B	�5B	�/B	�5B	�5B	�5B	�5B	�5B	�;B	�5B	�5B	�/B	�)B	�#B	�#B	�#B	�#B	�#B	�#B	�)B	�)B	�)B	�)B	�)B	�)B	�)B	�#B	�#B	�)B	�)B	�/B	�/B	�/B	�;B	�NB	�fB	�mB	�sB	�yB	�B	�B	��B	��B
B
bB
�B
49B
C�B
� B
�?B
��B
�mB#�Bq�B�B�9B��B�`B�B  B+B
=BhB&�B6FB<jBdZBz�B�7B��B��B��Bq�BbNB_;BcTBe`BhsBn�Bz�BÖB�HB�B�;B��BǮB�jB��B}�BVB7LB$�B��B�yB�B�!B�\B�B~�Bl�BB�B(�B8RBR�BK�B�B
��B
��B
�?B
�bB
ffB
1'B	�mB	ĜB	��B	bNB	J�B	<jB	,B	$�B	�B	\B	B��B��B�B�TB�B��BŢBB��B�}B�}B�qB�jB�wB��B��B��B��B�}B��B�}B�wB�qB�jB�dB�XB�RB�FB�?B�RB�LB�dB��BȴB��B�jB�qB�}BÖBƨBƨBÖB�FB�?B�?B�FB�FB�LB�XB�XB�RB�dBŢBŢBĜB��B�B�B�B��B��B��B��B��B�B�yB�fB�mB�`B�ZB�NB�BB�B��BǮBȴBǮB��B��B��B��B��B��B��B�B�B�B�B�
B�
B�
B�
B�
B�
B�B�B�B�B�B�#B�)B�/B�)B�)B�#B�#B�B�B�B�
B�B��B��B��B��B��B��B��B��B��B�B�B�)B�;B�NB�TB�ZB�ZB�TB�sB�B��B��B��B	  B	B	B	1B	
=B	JB	oB	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	$�B	7LB	?}B	E�B	M�B	O�B	R�B	VB	XB	[#B	[#B	YB	VB	YB	_;B	aHB	bNB	cTB	cTB	dZB	gmB	iyB	iyB	hsB	hsB	iyB	k�B	p�B	s�B	t�B	v�B	v�B	y�B	|�B	~�B	~�B	�B	�B	�%B	�JB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�3B	�-B	�B	�B	�B	�B	��B	��B	�B	�B	�B	�3B	�FB	�LB	�RB	�XB	�^B	�qB	��B	ÖB	ĜB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�B	�B	�#B	�)B	�BB	�ZB	�fB	�mB	�mB	�sB	�sB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
DB
	7B
hB
�B
�B
"�B
(�B
0!B
7LB
:^B
A�B
G�B
M�B
Q�B
XB
\)B
aHB
ffB
k�B
p�B
r�B
v�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B	�2B	�0B	�0B	�0B	�2B	�/B	�/B	�/B	�0B	�0B	�/B	�2B	�0B	�)B	�0B	�2B	�3B	�5B	�4B	�7B	�2B	�/B	�(B	�#B	�B	�B	�B	� B	�B	�B	�&B	�&B	�'B	�%B	�&B	�'B	�&B	�!B	� B	�$B	�"B	�*B	�,B	�)B	�6B	�GB	�aB	�gB	�pB	�rB	�B	�B	��B	��B
B
]B
�B
42B
C�B
�B
�3B
��B
�_B#�Bq�B��B�*B�qB�RB�B��BB
.BYB&�B69B<^BdOBz�B�)B��B��B�sBq�Bb@B_0BcCBeRBhfBn�Bz�BÆB�;B�B�*B��BǝB�]B��B}�BU�B7;B$�B��B�lB�B�B�IB�B~�Bl}BB~B(�B8BBR�BK�B�B
��B
˺B
�.B
�UB
fWB
1B	�eB	đB	��B	bIB	J�B	<gB	,B	$�B	�B	XB	B��B��B�B�SB�B��BšBB��B�|B�}B�pB�gB�yB��B��B��B��B�|B��B�{B�uB�oB�jB�cB�WB�PB�GB�=B�PB�IB�aB��BȵB��B�hB�nB�{BÕBƥBƦBÕB�EB�>B�>B�DB�FB�IB�VB�UB�PB�cBŠBşBĝB��B�B�B�B��B��B��B��B��B�B�sB�bB�iB�ZB�UB�JB�>B�BʾBǩBȰBǩBʿB��B��B��B��B��B��B�B��B�B� B�B�B�B�B�B�B�B�B�B�B�B�B�$B�)B�"B�%B�B�B�B�B�
B�B��B��B��B��B��B��B��B��B��B��B� B�B�%B�5B�JB�NB�RB�SB�OB�lB�B��B��B��B��B	B	B	+B	
4B	DB	fB	rB	xB	}B	�B	�B	�B	�B	�B	�B	�B	$�B	7@B	?sB	E�B	M�B	O�B	R�B	U�B	XB	[B	[B	YB	U�B	YB	_1B	a=B	bAB	cJB	cKB	dQB	gbB	imB	ilB	hfB	hhB	ikB	kzB	p�B	s�B	t�B	v�B	v�B	y�B	|�B	~�B	~�B	�B	�B	�B	�<B	�`B	�sB	�zB	�xB	��B	�xB	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�$B	�B	�B	��B	�B	��B	��B	��B	��B	�B	�B	�$B	�5B	�=B	�DB	�HB	�QB	�bB	�uB	ÇB	čB	ɫB	̾B	̾B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�2B	�JB	�VB	�\B	�^B	�aB	�bB	�fB	�dB	�mB	�wB	�|B	�~B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
�B
G�O�B
	%B
WB
�B
�B
"�B
(�B
0B
7:B
:KB
AwB
G�B
M�B
Q�B
X B
\B
a4B
fSB
kpB
p�B
r�B
v�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.18 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436352016080714363520160807143635  AO  ARCAADJP                                                                    20150518192036    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150518192036  QCP$                G�O�G�O�G�O�0               AO  ARGQQCPL                                                                    20150518192036  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143635  IP                  G�O�G�O�G�O�                