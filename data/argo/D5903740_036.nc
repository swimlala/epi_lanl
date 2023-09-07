CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:08:23Z AOML 3.0 creation; 2016-06-01T00:08:11Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20140721230823  20160531170811  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               $A   AO  4055_7112_036                   2C  D   APEX                            5374                            041511                          846 @ֆ�y��1   @ֆ�P���@:,�C���c#+I�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    $A   A   A   @�ff@�33A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/fD/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  DtffDyy�D���D�L�D��3D�ɚD�	�D�<�D�y�D�ɚD�	�D�P D���D��3D�	�D�VfDچfD๚D��D�P D�i�D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�(�@���A�HA"�HAB�HAb�HA�p�A�p�A�p�A�p�A�p�A�p�A�p�A�p�B �RB�RB�RB�RB �RB(�RB0�RB8�RB@�RBH�RBP�RBX�RB`�RBh�RBp�RBx�RB�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)C .C.C.C.C.C
.C.C.C.C.C.C.C.C.C.C.C .C".C$.C&.C(.C*.C,.C..C0.C2.C4.C6.C8.C:.C<.C>.C@.CB.CD.CF.CH.CJ.CL.CN.CP.CR.CT.CV.CX.CZ.C\.C^.C`.Cb.Cd.Cf.Ch.Cj.Cl.Cn.Cp.Cr.Ct.Cv.Cx.Cz.C|.C~.C�
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
C�#�C�
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
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dtq�Dy�D���D�R�D���D��]D�]D�B�D�]D��]D�]D�U�D���D���D�]D�\)Dڌ)D�]D��D�U�D�o]D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A��#A���A���A���A���A���A�ƨA�ĜA�A���A���A�ƨA��RA��!A��A���A���A���A���A��DA��A�|�A�x�A�p�A�ffA�ZA�Q�A�A�A�oA�%A�  A���A��A��A��mA��HA��A��
A���A���A���A�t�A�^5A�;dA��A��A�|�A���A���A��^A��-A���A��#A��A���A�ƨA��9A��DA�$�A���A�hsA�
=A��RA�A�v�A��A��-A���A�?}A���A���A� �A��uA��A�p�A�A�A���A�n�A��A���A�?}A�oA�z�A�hsA���A��A��uA�9XA�5?A�Q�A�33A���A�bNA��uA�  A��PA���A�~�A��A���A��yA��A���A��A�=qA���A~��A}?}A{��Az�AyhsAs�hAm�Aj��Ai��AgƨAdjA`=qA_t�A^v�A]�#A\�AY�;AY7LAXE�AV-AT9XAR�\AR  AQ%APjAN�AK�FAJ��AHZAGK�AGdZAG33AF��AE�FAD��ADA�AC�^AC�AB~�AA�^AAO�A@�RA?�A?VA>r�A=�A<��A< �A:��A9��A9|�A8E�A7&�A5XA45?A2��A1VA0�RA0��A0ȴA0-A-��A,n�A+�A*��A)��A)p�A)`BA(��A( �A'
=A&r�A%�7A%�A$�jA$�uA#?}A �AG�AVA��Ap�AȴA�;AoA�!A�A�A;dA�RAffA�;A�AM�A��A$�A�`A{AA$�A��Ap�A33A�jAv�AXA
�9A
{A	x�A	hsA	x�A��A7LAr�A��AXA��AA&�A�;A�PAK�A%@��@�~�@�@�V@� �@��@��9@��@��@�R@��@��D@��m@�!@���@�`B@��`@�r�@�t�@陚@���@�O�@�P@�+@�!@���@ߝ�@��@��y@�^5@���@�S�@�-@�bN@�C�@֧�@���@�V@� �@��H@�M�@�b@�J@���@��m@�o@�-@�@�p�@���@��
@�+@�V@�O�@��m@��m@öF@Å@+@�&�@��@�9X@��F@�dZ@�ȴ@��@���@��;@�ȴ@�=q@��^@���@���@���@���@��@���@�(�@��F@�33@�V@�J@��^@�/@��@�1@��@�M�@�J@��#@�X@���@� �@���@�
=@��@�?}@�z�@�;d@��\@��@��`@��D@�I�@���@���@��@��h@��9@� �@��
@�;d@��H@��@���@��\@�V@��h@��`@��9@���@��@�9X@��P@�C�@�+@�ff@�{@���@�hs@��@��`@��9@��@�z�@�1@��F@���@�K�@���@�-@���@�O�@���@��9@�9X@�b@���@��w@��@��@��R@��\@�E�@�@���@��7@�G�@��@��j@��D@� �@��F@�\)@���@���@���@���@��7@�/@�z�@�Z@�A�@��@��!@���@���@�`B@�&�@��`@��9@�r�@�(�@�b@��
@���@�|�@�t�@�\)@�"�@��R@�v�@�=q@�J@�@�7L@�&�@�%@���@��@�1@���@�t�@�dZ@�K�@�"�@���@��@��!@��+@�ff@�V@�-@�@��#@���@���@��-@��@�V@���@��`@��j@��@��D@�bN@�A�@�(�@�  @K�@~��@~��@~�+@~ff@~@}�h@}`B@|�@|��@|9X@{dZ@{C�@z�@{"�@{dZ@{C�@z�\@{33@{33@{S�@z�H@y��@yX@xbN@x �@xb@x  @w��@u��@m@d1@\�j@XbN@R��@K�m@E��@@A�@;C�@6E�@0�9@+S�@&{@ bN@o@@�`@�
@��@p�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A��#A���A���A���A���A���A�ƨA�ĜA�A���A���A�ƨA��RA��!A��A���A���A���A���A��DA��A�|�A�x�A�p�A�ffA�ZA�Q�A�A�A�oA�%A�  A���A��A��A��mA��HA��A��
A���A���A���A�t�A�^5A�;dA��A��A�|�A���A���A��^A��-A���A��#A��A���A�ƨA��9A��DA�$�A���A�hsA�
=A��RA�A�v�A��A��-A���A�?}A���A���A� �A��uA��A�p�A�A�A���A�n�A��A���A�?}A�oA�z�A�hsA���A��A��uA�9XA�5?A�Q�A�33A���A�bNA��uA�  A��PA���A�~�A��A���A��yA��A���A��A�=qA���A~��A}?}A{��Az�AyhsAs�hAm�Aj��Ai��AgƨAdjA`=qA_t�A^v�A]�#A\�AY�;AY7LAXE�AV-AT9XAR�\AR  AQ%APjAN�AK�FAJ��AHZAGK�AGdZAG33AF��AE�FAD��ADA�AC�^AC�AB~�AA�^AAO�A@�RA?�A?VA>r�A=�A<��A< �A:��A9��A9|�A8E�A7&�A5XA45?A2��A1VA0�RA0��A0ȴA0-A-��A,n�A+�A*��A)��A)p�A)`BA(��A( �A'
=A&r�A%�7A%�A$�jA$�uA#?}A �AG�AVA��Ap�AȴA�;AoA�!A�A�A;dA�RAffA�;A�AM�A��A$�A�`A{AA$�A��Ap�A33A�jAv�AXA
�9A
{A	x�A	hsA	x�A��A7LAr�A��AXA��AA&�A�;A�PAK�A%@��@�~�@�@�V@� �@��@��9@��@��@�R@��@��D@��m@�!@���@�`B@��`@�r�@�t�@陚@���@�O�@�P@�+@�!@���@ߝ�@��@��y@�^5@���@�S�@�-@�bN@�C�@֧�@���@�V@� �@��H@�M�@�b@�J@���@��m@�o@�-@�@�p�@���@��
@�+@�V@�O�@��m@��m@öF@Å@+@�&�@��@�9X@��F@�dZ@�ȴ@��@���@��;@�ȴ@�=q@��^@���@���@���@���@��@���@�(�@��F@�33@�V@�J@��^@�/@��@�1@��@�M�@�J@��#@�X@���@� �@���@�
=@��@�?}@�z�@�;d@��\@��@��`@��D@�I�@���@���@��@��h@��9@� �@��
@�;d@��H@��@���@��\@�V@��h@��`@��9@���@��@�9X@��P@�C�@�+@�ff@�{@���@�hs@��@��`@��9@��@�z�@�1@��F@���@�K�@���@�-@���@�O�@���@��9@�9X@�b@���@��w@��@��@��R@��\@�E�@�@���@��7@�G�@��@��j@��D@� �@��F@�\)@���@���@���@���@��7@�/@�z�@�Z@�A�@��@��!@���@���@�`B@�&�@��`@��9@�r�@�(�@�b@��
@���@�|�@�t�@�\)@�"�@��R@�v�@�=q@�J@�@�7L@�&�@�%@���@��@�1@���@�t�@�dZ@�K�@�"�@���@��@��!@��+@�ff@�V@�-@�@��#@���@���@��-@��@�V@���@��`@��j@��@��D@�bN@�A�@�(�@�  @K�@~��@~��@~�+@~ff@~@}�h@}`B@|�@|��@|9X@{dZ@{C�@z�@{"�@{dZ@{C�@z�\@{33@{33@{S�@z�H@y��@yX@xbN@x �@xb@x  @w��@u��@m@d1@\�j@XbN@R��@K�m@E��@@A�@;C�@6E�@0�9@+S�@&{@ bN@o@@�`@�
@��@p�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BɺBɺBȴBǮBŢBĜBĜBÖBBBBBBBĜBƨBǮB��B��B��B��BǮB�jB�'B�BÖB��B�B�/B�;B�5B�5B�/B�B��BÖB�qB�9B�B��B�Bm�BS�BD�B5?B,B(�B�B��B�yB�;BĜB�?B�B��B��BffBI�B9XB �B��B�B�5B�
B��B��B�hBy�Bl�BYB,BuB%B
��B
�B
�B
�?B
��B
�=B
v�B
N�B
5?B
�B
JB	��B	�B	�/B	�B	z�B	ffB	[#B	J�B	2-B	�B	�B	(�B	49B	1'B	$�B	�B	�B	PB	B��B��B��B�B�B�;B�#B�
B�)B�HB�BB�5B�B�
B�B�
B��B��B��B��B��BȴBƨBƨBŢBB�qB�9B�B�B�B��B�{B�hB�PB�1B�JB�oB�uB�oB�Bw�Bo�Bn�Bq�Bu�B�B�JB�DB�+B�B� B|�Bz�Bx�Bq�BcTBbNB_;B[#BW
BT�BP�BM�BL�BL�BJ�BI�BH�BF�BE�BC�BB�B@�B;dB7LB2-B,B(�B)�B(�B'�B&�B$�B �B�B�B�B�B$�B!�B�B�B�B�B�B�B�B�B�B�B�BuBoBbBbB\BbBVBPB
=B	7B1B1B+B1B	7B
=B
=B
=B
=B	7B
=B
=BPBPBPBVB\B\B\BVBPBPBPBVBVB\B\B\BVB\BVBVBhBoBuB{B{B{B{B�B�B�B�B�B�B�B�B�B�B �B�B!�B!�B"�B"�B#�B%�B'�B)�B+B,B-B0!B2-B49B49B5?B9XB:^B;dB>wB>wB?}B@�BB�BC�BG�BI�BJ�BK�BL�BN�BP�BQ�BR�BW
BZB\)B`BBcTBe`BjBk�Bl�Bm�Bq�Bt�Bt�Bv�By�B{�B~�B�B�B�%B�%B�%B�+B�DB�JB�JB�PB�bB�uB��B�{B��B��B��B��B��B��B��B��B��B��B�B�B�B�'B�9B�?B�RB�XB�dB�qB�}B��B��BÖBŢBǮBȴB��B��B��B��B��B��B��B�B�B�)B�;B�NB�ZB�B�B�B�B��B��B��B��B	B	PB	\B	hB	uB	�B	�B	�B	�B	�B	�B	!�B	#�B	#�B	$�B	&�B	+B	-B	/B	0!B	2-B	7LB	8RB	9XB	;dB	<jB	A�B	F�B	H�B	I�B	J�B	L�B	N�B	O�B	Q�B	S�B	VB	VB	YB	[#B	]/B	^5B	^5B	_;B	`BB	e`B	ffB	gmB	iyB	jB	k�B	l�B	m�B	n�B	o�B	q�B	s�B	u�B	v�B	w�B	y�B	{�B	{�B	}�B	}�B	� B	�B	�B	�B	�B	�%B	�+B	�=B	�PB	�PB	�VB	�\B	�\B	�hB	�uB	�{B	�{B	�{B	��B	��B	B	�TB	�B
  B
DB
�B
#�B
-B
49B
;dB
C�B
J�B
P�B
W
B
^5B
cTB
hsB
m�B
s�B
v�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B˾B��B˾B˾B˽B˽BʸBʸBʴBɱBɱBȯBǨBŗBĐBčBÑBBBBBBBĐBƝBǥBʸBʷBʶBʷBǧB�`B�B�BÑB��B�B�%B�4B�,B�,B�(B�B��BÐB�eB�1B��B�uB�Bm�BS�BD�B51B+�B(�B�B��B�mB�,BċB�2B�B��B��BfWBI�B9FB �B��B�oB�'B��B�pB��B�XBy�Bl}BYB+�BdBB
��B
�B
��B
�.B
��B
�.B
v�B
N�B
54B
�B
@B	��B	�B	�$B	�B	z�B	f]B	[ B	J�B	2*B	�B	�B	(�B	47B	1%B	$�B	�B	�B	NB	B��B��B��B�B�B�;B�#B�	B�)B�GB�AB�4B�B�	B�B�	B��B��B��B��B��BȵBƦBƧBŠBB�pB�9B�B�B�B��B�{B�iB�TB�3B�KB�qB�uB�qB�Bw�Bo�Bn�Bq�Bu�B�B�JB�EB�,B�B�B|�Bz�Bx�Bq�BcVBbOB_>B[&BWBUBP�BM�BL�BL�BJ�BI�BH�BF�BE�BC�BB�B@�B;hB7QB22B,B(�B*B(�B'�B&�B$�B �B�B�B�B�B$�B!�B�B�B�B�B�B�B�B�B�B{BnByBYBgBMB_BKB?BUB
&B	#BBB0BB	<B
&B
'B
'B
AB	!B
&B
'BUB8BTB@B^BGB`BZB8B:B9B@B\B_BDB_BAB_B>B=BQBqB^B~BeB~B}B�BoBoB�B�B�B�B�B�B�B �B�B!�B!�B"�B"�B#�B%�B'�B)�B+B,	B-B0!B2,B4;B4=B5AB9ZB:`B;cB>vB>xB?}B@�BB�BC�BG�BI�BJ�BK�BL�BN�BP�BQ�BR�BWBZB\*B`ABcSBe`Bj|Bk�Bl�Bm�Bq�Bt�Bt�Bv�By�B{�B~�B�B�B�#B� B�%B�*B�@B�DB�EB�MB�^B�qB�|B�vB��B��B��B��B��B��B��B��B��B��B��B�B�B�!B�4B�9B�LB�SB�]B�iB�vB�|B��BÐBŝBǧBȯBʹB��B��B��B��B��B��B��B�B�$B�1B�HB�QB�B�B�B�B��B��B��B��B	B	HB	RB	aB	kB	wB	�B	�B	�B	�B	�B	!�B	#�B	#�B	$�B	&�B	*�B	-B	/B	0B	2#B	7BB	8GB	9KB	;YB	<^B	A~B	F�B	H�B	I�B	J�B	L�B	N�B	O�B	Q�B	S�B	U�B	U�B	YB	[B	]"B	^*B	^)B	_0B	`5B	eQB	fWB	g^B	ikB	jtB	kxB	l~B	m�B	n�B	o�B	q�B	s�B	u�B	v�B	w�B	y�B	{�B	{�B	}�B	}�B	�B	��B	��B	�B	�B	�B	�B	�-B	�CB	�AB	�FB	�NB	�MB	�YB	�fB	�mB	�mB	�oB	�wB	��B	�B	�DB	�B	��B
4B
�B
#�B
,�B
4(B
;PB
C�B
J�B
P�B
V�B
^"B
c@B
h_B
m~B
s�B
v�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.18 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708112016053117081120160531170811  AO  ARCAADJP                                                                    20140721230823    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230823  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230823  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170811  IP                  G�O�G�O�G�O�                