CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:08:36Z AOML 3.0 creation; 2016-06-01T00:08:15Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20140721230836  20160531170815  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               =A   AO  4055_7112_061                   2C  D   APEX                            5374                            041511                          846 @�����1   @��3�?�@:l1&�y�dO�;dZ1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    =A   A   A   @���@���A   A   A@  A`  A�  A�  A�  A�  A�  A�33A�33A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  BpffBw��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:�C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D fD � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6y�D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dy�3D�fD�Y�D���D��fD�	�D�I�D�y�D�ɚD�fD�33D�s3Dǩ�D�fD�9�Dړ3D���D���D�,�D�\�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�\)@\A�HA"�HAB�HAb�HA�p�A�p�A�p�A�p�A�p�AУ�A��A�p�B �RB�RB�RB�RB �RB(�RB0�RB8�RB@�RBH�RBP�RBX�RB`�RBh�RBq�BxQ�B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B܏\B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)C .C.C.C.C.C
.C.C.C.C.C.C.C.C.C.C.C .C".C$.C&.C(.C*.C,.C..C0.C2.C4.C6.C8.C:G�C<.C>.C@.CB.CD.CF.CH.CJ.CL.CN.CP.CR.CT.CV.CX.CZ.C\.C^.C`.Cb.Cd.Cf.Ch.Cj.Cl.Cn.Cp.Cr.Ct.Cv.Cx.Cz.C|.C~.C�
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
C�#�C�
C�
C�
C�
C�
=C�
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
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6�D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Dt�RDy��D�)D�_]D��]D��)D�]D�O]D�]D��]D�)D�8�D�x�Dǯ]D�)D�?]Dژ�D�ҐD��D�2�D�b�D��]111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A���A���A�%A�
=A�JA�1A�1A�1A�1A�
=A�bA�JA�oA��A��A�&�A�(�A�(�A�"�A�"�A�"�A�$�A�+A�/A�1'A�1'A��TAʰ!A��A��\A���A�+A�XA��RA�=qA���A�C�A�(�A�G�A���A��9A�G�A��jA�(�A�ƨA��RA��;A��\A��yA���A�x�A�S�A�ZA�oA��A�M�A�-A���A��A�n�A��RA�`BA���A�O�A��FA��mA�$�A�bA�p�A�C�A��A��#A���A���A��
A��A��hA��hA��+A��!A��A�x�A��A���A�G�A�+A�{A�hsA�{A~{A|�`A{��Az�Axv�As�-Ap^5Am�wAm�Al��Ak�Aj1Ag�FAe�FAd��Ac�Ab�\Aa\)AaG�A`��A_"�A["�AY/AXZAW��AW��AW%AU��ATffARz�AR-AQK�APz�AOƨAN��AL��AI�wAH�jAG
=AD�`AD9XAC?}ABĜAB1'AA�FAA+A@�A=�A=oA<I�A;K�A;VA:1A9�7A9"�A8=qA7`BA7oA6{A4�DA3�FA2{A1K�A0�/A0z�A/��A//A.�jA,�A,ffA,�A+O�A*n�A)�A)�hA)�A)l�A)�A(Q�A'�PA';dA&�HA$�A"�A!`BA �\A&�A7LA�A$�A+A1'AoA�7A��A��A�PA�A��A�yA�A�9Az�A�A33A�AffA�7Ap�AC�A�AE�A�7A�HAjAhsA
��A
��A	�#A	��AA�A��AAS�A?}A �A Q�A �@�+@�?}@��@��@���@��7@�"�@��`@�M�@�1'@@�@�P@�n�@畁@�n�@���@��@�z�@�1'@�w@��T@݉7@�dZ@ڏ\@���@ج@� �@��@�@�@���@���@�v�@�{@պ^@�/@Դ9@�I�@ӍP@Ѳ-@� �@�hs@�K�@�V@��/@�I�@��m@�\)@�33@�@���@�J@ŉ7@�j@�@�p�@�l�@�{@���@��@��D@���@�
=@���@�E�@�/@���@��u@�b@��@���@�/@��/@���@���@�r�@�(�@��;@���@�t�@��@�ff@��@��#@�O�@�Z@�o@���@�@��@�bN@�I�@��m@���@�K�@�o@�K�@�@��@�ff@�~�@�n�@�@���@�7L@�%@���@�I�@��P@�5?@���@��@���@��P@�\)@��@�v�@�^5@��^@�O�@�V@���@�(�@��;@�dZ@���@��R@�^5@�5?@��@��@�p�@���@�I�@��@�l�@�K�@�+@�o@��y@�E�@��@� �@��w@�S�@�
=@���@��R@��\@�@�?}@���@��9@���@�|�@��@���@��@���@��-@�G�@�Z@���@��@�
=@���@���@��m@�o@��@���@�n�@�n�@�n�@�ff@�V@�V@�-@���@��@�X@�G�@��@�%@���@��@�r�@�bN@�Q�@�A�@�A�@�(�@�b@��
@���@��@�dZ@�K�@���@�ȴ@���@�~�@�^5@�{@���@�p�@�V@��@�Ĝ@�9X@�1@�ƨ@�|�@���@��H@�ȴ@��!@��+@�M�@�$�@�@�p�@�O�@�?}@���@���@��u@��D@��D@��@�bN@�@+@�@~��@~V@}�@|�/@|(�@{�m@{��@{t�@{t�@{dZ@{C�@{"�@{@z��@zn�@z-@y��@x�`@xr�@x  @w�@v��@vff@vV@v5?@v{@u��@u�@t�/@s�m@r��@r-@p��@p�9@pbN@pQ�@l�@d�@_�w@V�@P�`@J��@E�@;o@7;d@/��@.��@-O�@$��@ Q�@-@1@|�@�F@1'@K�@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A���A���A���A�%A�
=A�JA�1A�1A�1A�1A�
=A�bA�JA�oA��A��A�&�A�(�A�(�A�"�A�"�A�"�A�$�A�+A�/A�1'A�1'A��TAʰ!A��A��\A���A�+A�XA��RA�=qA���A�C�A�(�A�G�A���A��9A�G�A��jA�(�A�ƨA��RA��;A��\A��yA���A�x�A�S�A�ZA�oA��A�M�A�-A���A��A�n�A��RA�`BA���A�O�A��FA��mA�$�A�bA�p�A�C�A��A��#A���A���A��
A��A��hA��hA��+A��!A��A�x�A��A���A�G�A�+A�{A�hsA�{A~{A|�`A{��Az�Axv�As�-Ap^5Am�wAm�Al��Ak�Aj1Ag�FAe�FAd��Ac�Ab�\Aa\)AaG�A`��A_"�A["�AY/AXZAW��AW��AW%AU��ATffARz�AR-AQK�APz�AOƨAN��AL��AI�wAH�jAG
=AD�`AD9XAC?}ABĜAB1'AA�FAA+A@�A=�A=oA<I�A;K�A;VA:1A9�7A9"�A8=qA7`BA7oA6{A4�DA3�FA2{A1K�A0�/A0z�A/��A//A.�jA,�A,ffA,�A+O�A*n�A)�A)�hA)�A)l�A)�A(Q�A'�PA';dA&�HA$�A"�A!`BA �\A&�A7LA�A$�A+A1'AoA�7A��A��A�PA�A��A�yA�A�9Az�A�A33A�AffA�7Ap�AC�A�AE�A�7A�HAjAhsA
��A
��A	�#A	��AA�A��AAS�A?}A �A Q�A �@�+@�?}@��@��@���@��7@�"�@��`@�M�@�1'@@�@�P@�n�@畁@�n�@���@��@�z�@�1'@�w@��T@݉7@�dZ@ڏ\@���@ج@� �@��@�@�@���@���@�v�@�{@պ^@�/@Դ9@�I�@ӍP@Ѳ-@� �@�hs@�K�@�V@��/@�I�@��m@�\)@�33@�@���@�J@ŉ7@�j@�@�p�@�l�@�{@���@��@��D@���@�
=@���@�E�@�/@���@��u@�b@��@���@�/@��/@���@���@�r�@�(�@��;@���@�t�@��@�ff@��@��#@�O�@�Z@�o@���@�@��@�bN@�I�@��m@���@�K�@�o@�K�@�@��@�ff@�~�@�n�@�@���@�7L@�%@���@�I�@��P@�5?@���@��@���@��P@�\)@��@�v�@�^5@��^@�O�@�V@���@�(�@��;@�dZ@���@��R@�^5@�5?@��@��@�p�@���@�I�@��@�l�@�K�@�+@�o@��y@�E�@��@� �@��w@�S�@�
=@���@��R@��\@�@�?}@���@��9@���@�|�@��@���@��@���@��-@�G�@�Z@���@��@�
=@���@���@��m@�o@��@���@�n�@�n�@�n�@�ff@�V@�V@�-@���@��@�X@�G�@��@�%@���@��@�r�@�bN@�Q�@�A�@�A�@�(�@�b@��
@���@��@�dZ@�K�@���@�ȴ@���@�~�@�^5@�{@���@�p�@�V@��@�Ĝ@�9X@�1@�ƨ@�|�@���@��H@�ȴ@��!@��+@�M�@�$�@�@�p�@�O�@�?}@���@���@��u@��D@��D@��@�bN@�@+@�@~��@~V@}�@|�/@|(�@{�m@{��@{t�@{t�@{dZ@{C�@{"�@{@z��@zn�@z-@y��@x�`@xr�@x  @w�@v��@vff@vV@v5?@v{@u��@u�@t�/@s�m@r��@r-@p��@p�9@pbN@pQ�@l�@d�@_�w@V�@P�`@J��@E�@;o@7;d@/��@.��@-O�@$��@ Q�@-@1@|�@�F@1'@K�@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBF�BF�BE�BE�BE�BE�BE�BE�BE�BE�BE�BE�BF�BE�BD�BD�BE�BE�BE�BD�BD�BD�BD�BE�BE�BE�BD�B=qB49B�BE�B1'B!�B"�B$�B"�B�B�B
=BB  B��B�B�TB�/B�B��B�-B��B��B��B��B�By�Bt�BVBG�BE�B@�B49B�BVB%B��B�B�TB��B�XB��B��B�JB�+B�B|�BiyBJ�B%�BB
�mB
�B
��B
��B
�?B
��B
�+B
t�B
dZB
T�B
J�B
D�B
5?B
-B
%�B
�B
+B	�mB	��B	ŢB	B	�wB	�FB	�B	��B	�{B	�\B	�7B	�B	z�B	y�B	t�B	ffB	S�B	J�B	H�B	F�B	E�B	@�B	:^B	2-B	)�B	(�B	%�B	"�B	�B	�B	JB	B��B��B�sB�HB�
B��B��B��B��BĜBĜBǮBǮBŢBĜB��B��B�wB�jB�RB�RB�9B�B�B��B��B��B��B��B��B��B��B��B��B�oB�\B�PB�JB�JB�DB�7B�B�B�B}�Bs�Bp�Bm�Bk�Be`BbNBaHB^5B[#BW
BS�BQ�BP�BN�BN�BM�BM�BM�BL�BL�BJ�BH�BE�BB�B@�B?}B>wB>wB<jB:^B8RB7LB5?B33B2-B1'B/B-B'�B$�B �B�B�B�B�B�B�B�B�B{BuBbB\BPBDBDB
=B
=B	7B+B1B1B1B1B1B+B%BBB+B%B1B1B+B	7B	7B	7B1B1B1B1B1B	7B	7B1B+B	7B	7BDBVB\BhBoBuB{B{B{B{B�B�B�B�B�B�B�B�B�B �B!�B#�B$�B$�B'�B(�B)�B+B-B1'B2-B33B33B49B49B5?B6FB6FB5?B6FB8RB:^B:^B<jB>wBB�BC�BD�BG�BH�BI�BI�BI�BJ�BJ�BJ�BJ�BJ�BK�BJ�BJ�BK�BK�BK�BK�BK�BK�BL�BQ�BW
BYB]/B^5B`BBbNBgmBjBm�Bo�Bp�Bp�Bs�Bt�Bv�By�Bz�B|�B}�B}�B�B�B�+B�DB�VB�bB�hB�hB�hB�oB�{B��B��B��B��B��B�B�B�B�'B�LB�XB�^B�}BÖBǮB��B��B��B��B��B�B�5B�BB�TB�B�B��B	B	B	B	%B	%B	%B	%B	%B	+B	1B		7B	PB	\B	\B	hB	oB	uB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	 �B	!�B	%�B	'�B	(�B	)�B	+B	-B	/B	2-B	6FB	7LB	8RB	=qB	@�B	B�B	E�B	I�B	J�B	L�B	L�B	N�B	Q�B	R�B	XB	\)B	]/B	^5B	`BB	aHB	dZB	dZB	dZB	dZB	ffB	gmB	k�B	k�B	n�B	o�B	q�B	x�B	|�B	~�B	�B	�B	�B	�B	�B	�B	�B	�%B	�1B	�7B	�JB	�bB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�3B	�3B	�?B	�?B	��B	�;B	�B	��B
	7B
{B
�B
.B
49B
?}B
A�B
B�B
K�B
P�B
W
B
`BB
e`B
k�B
p�B
q�B
u�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  BF�BF�BE�BE�BE�BE�BE�BE�BE�BE�BE�BE�BF�BE�BD�BD�BE�BE�BE�BD�BD�BD�BD�BE�BE�BE�BD�B=lB47B�BE�B1B!�B"�B$�B"�B�BB
3B B��B��B�xB�EB� B�BʱB� B��B��B��B�{B��By�Bt�BU�BG�BE�B@pB4&B�BCBB��B�B�@B��B�DB��B��B�9B�B�B|�BifBJ�B%�BB
�`B
�B
ʰB
�vB
�.B
��B
�B
t�B
dIB
T�B
J�B
D�B
53B
-B
%�B
�B
"B	�bB	��B	řB	B	�oB	�=B	�B	��B	�tB	�TB	�/B	��B	z�B	y�B	t�B	f`B	S�B	J�B	H�B	F�B	E�B	@B	:YB	2(B	)�B	(�B	%�B	"�B	�B	�B	FB	B��B��B�sB�GB�B��B��B��B��BĝBĝBǭBǬBŠBĝB��B��B�wB�jB�RB�RB�9B�B�B��B��B��B��B��B��B��B��B��B��B�qB�^B�RB�JB�IB�GB�9B�!B�B�	B}�Bs�Bp�Bm�Bk�BecBbQBaKB^9B['BWBS�BQ�BP�BN�BN�BM�BM�BM�BL�BL�BJ�BH�BE�BB�B@�B?�B>|B>|B<oB:bB8UB7QB5CB37B22B1,B/B-B'�B$�B �B�B�B�B�B�BsBlBiBBzBLB`B:BFB,B
)B
%B	!BBBBBBB1B*BB
BBBBB.B	;B	B	BBBBBB	=B	 BBB	9B	:BHBZB_BPBVBxB}B~BdBBgB�B�B�B�B�B�B�B�B �B!�B#�B$�B$�B'�B(�B)�B+B-B1'B2,B35B35B4;B4<B5@B6FB6FB5AB6FB8SB:^B:^B<iB>vBB�BC�BD�BG�BH�BI�BI�BI�BJ�BJ�BJ�BJ�BJ�BK�BJ�BJ�BK�BK�BK�BK�BK�BK�BL�BQ�BW
BYB])B^2B`@BbLBgjBj~Bm�Bo�Bp�Bp�Bs�Bt�Bv�By�Bz�B|�B}�B}�B�B�B�)B�@B�PB�_B�eB�dB�fB�lB�vB��B��B��B��B��B�B�B�B�!B�FB�SB�[B�wBÑBǩB��B��B��B��B��B�B�/B�8B�OB�|B�B��B	 �B	 �B	B	B	B	B	B	B	"B	'B		/B	GB	SB	TB	]B	eB	mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	 �B	!�B	%�B	'�B	(�B	)�B	*�B	-B	/B	2"B	69B	7@B	8FB	=eB	@xB	B�B	E�B	I�B	J�B	L�B	L�B	N�B	Q�B	R�B	XB	\B	]#B	^*B	`8B	a:B	dMB	dMB	dMB	dKB	fXB	g^B	kwB	kyB	n�B	o�B	q�B	x�B	|�B	~�B	��B	� B	� B	� B	�B	�B	�B	�B	�"B	�)B	�<B	�TB	�aB	�qB	�sB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�'B	�0B	�-B	�yB	�+B	�mB	��B
	'B
jB
�B
.B
4&B
?iB
AtB
B}B
K�B
P�B
V�B
`0B
eOB
krB
p�B
q�B
u�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.18 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708152016053117081520160531170815  AO  ARCAADJP                                                                    20140721230836    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230836  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230836  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170815  IP                  G�O�G�O�G�O�                