CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2016-06-08T02:15:28Z AOML 3.0 creation; 2016-08-07T21:51:30Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20160608021528  20160825183417  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               ~A   AO  5287_9017_126                   2C  D   APEX                            6529                            072314                          846 @ײ;�\��1   @ײ<WX�@0cS����dى7Kƨ1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    ~A   B   B   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B���C  C�C�fC  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dts3Dy` D��3D�FfD�� D���D�  D�0 D�vfD�� D�	�D�L�D�p D���D�	�D�,�D�` D��3D�3D�FfD��D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��R@�Q�A(�A((�AH(�Ah(�A�{A�{A�{A�{A�{A�{A�{A�{B
=B

=B
=B
=B"
=B*
=B2
=B:
=BB
=BJ
=BR
=BZ
=Bb
=Bj
=Br
=Bz
=B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�8RB�8RB�B�B�C h�C��C�)Ch�C��C
��C��C��C��C��C��C��C��C��C��C��C ��C"��C$��C&��C(��C*��C,��C.��C0��C2��C4��C6��C8��C:��C<��C>��C@��CB��CD��CF��CH��CJ��CL��CN��CP��CR��CT��CV��CX��CZ��C\��C^��C`��Cb��Cd��Cf��Ch��Cj��Cl��Cn��Cp��Cr��Ct��Cv��Cx��Cz��C|��C~��C�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�4{C�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHD  �D ��D �D��D �D��D �D��D �D�
D �D��D �D��D �D��D �D��D	 �D	��D
 �D
��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D�
D �D��D  �D ��D! �D!��D" �D"��D# �D#��D$ �D$��D% �D%��D& �D&��D' �D'��D( �D(��D) �D)��D* �D*��D+ �D+��D, �D,��D- �D-��D. �D.��D/ �D/��D0 �D0��D1 �D1��D2 �D2��D3 �D3��D4 �D4��D5 �D5��D6 �D6��D7 �D7��D8 �D8��D9 �D9��D: �D:��D; �D;��D< �D<��D= �D=��D> �D>��D? �D?��D@ �D@��DA �DA��DB �DB��DC �DC��DD �DD��DE �DE��DF �DF��DG �DG��DH �DH��DI �DI��DJ �DJ��DK �DK��DL �DL��DM �DM��DN �DN��DO �DO��DP �DP��DQ �DQ��DR �DR��DS �DS��DT �DT��DU �DU��DV �DV��DW �DW��DX �DX��DY �DY��DZ �DZ��D[ �D[��D\ �D\��D] �D]��D^ �D^��D_ �D_��D` �D`��Da �Da��Db �Db��Dc �Dc��Dd �Dd��De �De��Df �Df��Dg �Dg��Dh �Dh��Di �Di��Dj �Dj��Dk �Dk��Dl �Dl��Dm �Dm��Dn �Dn��Do �Do��Dp �Dp��Dq �Dq��Dr �Dr��Ds �Ds��Dt �Dt��Dy��D��D�V�D��RD��D�RD�@RD���D��RD��D�]D��RD��D��D�=D�pRD�ӅD��D�V�D�D�Ӆ11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��HA���AؓuA�|�A�XA�M�A�K�A�C�A�;dA�7LA��A��A�ĜAװ!A׬A׬AדuA�I�A��;A�x�A���A���AոRA�|�A�x�A�M�A�;dA�1'A�(�A��A�
=A�A��A��yA��HA���AԮA�~�A�-A��Aӥ�A�^5A�$�A���A�XAѩ�A��;A�bAϏ\AΑhA���A͟�A�ZȀ\AˑhA���A���Aȡ�A���A�A�"�A§�A��#A���A�1'A�v�A�33A��#A�jA��A��A��A���A��;A�bNA�VA�7LA���A�hsA�+A���A��A�/A�  A�VA���A��^A�ZA�VA��9A�5?A��hA�E�A�`BA�$�A�A�x�A��A��PA���A��!A��^A�ffA��A�Q�A�S�A��TA~��A}�A}�A{x�Ay�
Ax��Ax�Aw\)At�HAr=qAo��Am�Al�9Aj��Ad��A^�!A\1'AW��ATffARĜAP��AO�AOhsAM"�AK"�AIO�AG�AF^5AD�uAB�AA�A>�DA=�;A<�uA:^5A8��A8r�A8A�A8  A7��A7VA5�A3�wA1A0Q�A.�A+S�A)A)oA(ĜA(�A(5?A'��A%�^A$M�A#�A"jA!K�A M�At�Az�A�AQ�AdZA��AM�A��A~�A�hA��A�PAM�A�^A%A��A�
A��AoA�DA�Ax�A��AVA�PA|�A;dA
  A	�^A	�A	��A9XAZA�A��A��A��A�A"�A��A�A�9A��A�\A�+AA;dA ��A  �@���@���@�&�@�bN@��H@�G�@��m@�5?@��@�O�@���@�&�@���@�Q�@��
@�t�@�
=@���@�n�@��@�h@�hs@�&�@��`@�9@��;@��@�@�t�@�;d@�^5@��T@�X@�V@�j@�Z@�  @�ƨ@땁@�l�@�"�@�!@�ff@�j@�@畁@畁@�@��@�C�@��@�@�&�@�Ĝ@��@�;d@ݡ�@�/@�&�@ܬ@�Q�@�1@�  @�b@�(�@�I�@�Q�@�K�@ش9@׶F@� �@�z�@�r�@��y@ա�@�1'@љ�@�1@Ϯ@�S�@���@Ο�@ΰ!@·+@�$�@͙�@��@�bN@� �@��@�b@�1@�ƨ@�K�@��H@ʰ!@ʇ+@�v�@�M�@��T@�?}@���@��@��@Ƨ�@�ff@�$�@�`B@���@ģ�@ě�@�Z@��;@þw@���@\@�~�@�M�@���@���@���@��u@��P@�K�@��@��@���@�$�@�V@��u@��@�t�@�+@���@��@�E�@���@��-@�`B@��@�9X@���@�K�@��y@��!@��!@���@���@���@���@�E�@�5?@��@��h@���@�b@��w@��w@�|�@�
=@���@�5?@��@�p�@��j@�z�@�Z@�A�@� �@� �@��@�  @��m@�C�@�{@��@��-@��7@�hs@��@��@���@�Z@���@���@��@�O�@�%@��9@�r�@�1@���@�\)@�@���@�~�@�~�@�~�@�~�@�ff@�M�@�-@�J@��@���@��9@�@�ȴ@���@�n�@�^5@�-@�@��T@�?}@���@��D@�Z@�1@��@�|�@�;d@��@��y@�ȴ@�n�@�-@��@��@�hs@��@�Z@��w@�S�@�C�@�+@�o@��y@��\@�$�@��#@���@��-@��@�hs@��@��@��m@�\)@�C�@�+@��@�o@��H@��!@�=q@�@�p�@�7L@��@��@��@�b@��;@���@��@�\)@��@��@��@��^@��@�O�@�=q@�%@���@�@u�-@j-@`��@U�h@M�@E@<I�@5/@-`B@)�7@#@ff@�7@9X@�@��@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A��HA���AؓuA�|�A�XA�M�A�K�A�C�A�;dA�7LA��A��A�ĜAװ!A׬A׬AדuA�I�A��;A�x�A���A���AոRA�|�A�x�A�M�A�;dA�1'A�(�A��A�
=A�A��A��yA��HA���AԮA�~�A�-A��Aӥ�A�^5A�$�A���A�XAѩ�A��;A�bAϏ\AΑhA���A͟�A�ZȀ\AˑhA���A���Aȡ�A���A�A�"�A§�A��#A���A�1'A�v�A�33A��#A�jA��A��A��A���A��;A�bNA�VA�7LA���A�hsA�+A���A��A�/A�  A�VA���A��^A�ZA�VA��9A�5?A��hA�E�A�`BA�$�A�A�x�A��A��PA���A��!A��^A�ffA��A�Q�A�S�A��TA~��A}�A}�A{x�Ay�
Ax��Ax�Aw\)At�HAr=qAo��Am�Al�9Aj��Ad��A^�!A\1'AW��ATffARĜAP��AO�AOhsAM"�AK"�AIO�AG�AF^5AD�uAB�AA�A>�DA=�;A<�uA:^5A8��A8r�A8A�A8  A7��A7VA5�A3�wA1A0Q�A.�A+S�A)A)oA(ĜA(�A(5?A'��A%�^A$M�A#�A"jA!K�A M�At�Az�A�AQ�AdZA��AM�A��A~�A�hA��A�PAM�A�^A%A��A�
A��AoA�DA�Ax�A��AVA�PA|�A;dA
  A	�^A	�A	��A9XAZA�A��A��A��A�A"�A��A�A�9A��A�\A�+AA;dA ��A  �@���@���@�&�@�bN@��H@�G�@��m@�5?@��@�O�@���@�&�@���@�Q�@��
@�t�@�
=@���@�n�@��@�h@�hs@�&�@��`@�9@��;@��@�@�t�@�;d@�^5@��T@�X@�V@�j@�Z@�  @�ƨ@땁@�l�@�"�@�!@�ff@�j@�@畁@畁@�@��@�C�@��@�@�&�@�Ĝ@��@�;d@ݡ�@�/@�&�@ܬ@�Q�@�1@�  @�b@�(�@�I�@�Q�@�K�@ش9@׶F@� �@�z�@�r�@��y@ա�@�1'@љ�@�1@Ϯ@�S�@���@Ο�@ΰ!@·+@�$�@͙�@��@�bN@� �@��@�b@�1@�ƨ@�K�@��H@ʰ!@ʇ+@�v�@�M�@��T@�?}@���@��@��@Ƨ�@�ff@�$�@�`B@���@ģ�@ě�@�Z@��;@þw@���@\@�~�@�M�@���@���@���@��u@��P@�K�@��@��@���@�$�@�V@��u@��@�t�@�+@���@��@�E�@���@��-@�`B@��@�9X@���@�K�@��y@��!@��!@���@���@���@���@�E�@�5?@��@��h@���@�b@��w@��w@�|�@�
=@���@�5?@��@�p�@��j@�z�@�Z@�A�@� �@� �@��@�  @��m@�C�@�{@��@��-@��7@�hs@��@��@���@�Z@���@���@��@�O�@�%@��9@�r�@�1@���@�\)@�@���@�~�@�~�@�~�@�~�@�ff@�M�@�-@�J@��@���@��9@�@�ȴ@���@�n�@�^5@�-@�@��T@�?}@���@��D@�Z@�1@��@�|�@�;d@��@��y@�ȴ@�n�@�-@��@��@�hs@��@�Z@��w@�S�@�C�@�+@�o@��y@��\@�$�@��#@���@��-@��@�hs@��@��@��m@�\)@�C�@�+@��@�o@��H@��!@�=q@�@�p�@�7L@��@��@��@�b@��;@���@��@�\)@��@��@��@��^@��G�O�@�=q@�%@���@�@u�-@j-@`��@U�h@M�@E@<I�@5/@-`B@)�7@#@ff@�7@9X@�@��@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�yB
�ZB
�5B
�B
��B
��B
��B
��B
��B
��B
��B
ɺB
ɺB
ɺB
ɺB
ɺB
ȴB
ɺB
ɺB
ƨB
ŢB
ÖB
��B
�qB
�XB
�LB
�?B
�-B
�B
�B
�B
�!B
�B
�FB
B
ǮB
��B
�ZB
��B\B33BYB�bB��B�LB�/BB�B �B$�B%�B%�B&�B&�B&�B$�B!�B!�B�B�B�B�B1B��B��B�B�yB�NB��B�B_;BW
BO�B;dB'�B33BT�BP�B<jB#�BB
��B
��B
ĜB
�9B
��B
�bB
}�B
hsB
YB
C�B
1'B
+B
%�B
�B
bB

=B
B	��B	�B	�#B	��B	�wB	�FB	��B	�oB	n�B	\)B	=qB	.B	+B	 �B	�B	{B	1B	+B	PB	DB	%B	B��B��B�B�mB�NB�/B�)B�#B�B�B�
B��B��B��BɺBƨBB��B�}B�}B�}B�wB�qB�dB�XB�LB�FB�9B�9B�?B�?B�3B�'B�'B�'B�9B�FB�RB�qB�dB�FB�RB�9B�XB�^B��BǮBȴB��B�yB�B��B		7B	DB	JB	{B	�B	#�B	&�B	&�B	'�B	(�B	)�B	)�B	+B	+B	/B	<jB	D�B	D�B	C�B	B�B	A�B	A�B	A�B	C�B	J�B	R�B	XB	\)B	^5B	`BB	`BB	bNB	dZB	ffB	o�B	y�B	�+B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�-B	�9B	�?B	�?B	�LB	�dB	�wB	�}B	��B	ÖB	ƨB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ǮB	ĜB	ǮB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	ɺB	ȴB	��B	��B	��B	��B	ɺB	ÖB	�XB	�FB	�?B	�?B	�FB	�RB	�^B	�dB	�qB	��B	��B	ÖB	ĜB	ĜB	ĜB	ĜB	ŢB	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�
B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�)B	�5B	�5B	�;B	�TB	�TB	�TB	�TB	�TB	�TB	�TB	�TB	�NB	�HB	�;B	�/B	�)B	�#B	�B	�B	�B	�B	�B	�B	�
B	�
B	�B	��B	��B	��B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�)B	�/B	�;B	�BB	�NB	�fB	�yB	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
%B
+B
+B
+B
+B
1B
1B
1B
1B
1B
1B
1B
+B
B
  B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
1B
	7B
	7B
	7B
	7B
	7B

=B
DB
DB
DB
DB
DB
DB
DB
JB
PB
VB
VB
VB
VB
VB
VB
VB
VB
\B
bB
bB
bB
bB
hB
hB
oB
oB
hB
oB
oB
uB
uB
uB
{B
�B
�B
#�B
(�B
.B
49B
8RB
<jB
D�B
I�B
M�B
T�B
[#B
aHB
e`B
jB
o�B
s�B
x�B
}�B
� B
~�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B
�[B
�WB
�`B
�^B
�aB
�bB
�cB
�cB
�_B
�_B
�YB
�^B
�cB
�aB
�bB
�`B
�TB
�3B
�B
��B
��B
εB
ͪB
ˡB
ϻB
̨B
ʞB
ɕB
ɕB
ɘB
ɒB
ɔB
ȎB
ɖB
ɔB
ƀB
�|B
�qB
�^B
�KB
�1B
�)B
�B
�B
��B
��B
��B
��B
��B
�"B
�iB
ǇB
ϹB
�2B
��B4B3BX�B�<B��B�!B�B�BsB �B$�B%�B%�B&�B&�B&�B$�B!�B!�B�B�B�BaBB��B��B�tB�IB�BзB��B_BV�BO�B;6B'�B3BT�BP�B<:B#�B�B
ͥB
ϰB
�nB
�	B
��B
�3B
}�B
hCB
X�B
ChB
0�B
*�B
%�B
yB
6B

B
�B	��B	�aB	��B	˜B	�OB	�B	��B	�DB	nmB	\ B	=HB	-�B	*�B	 �B	B	WB	
B	B	&B	B	�B	 �B��B��B�eB�EB�(B�	B�B��B��B��B��B��BпB̥BɔBƁB�iB�\B�WB�TB�UB�OB�HB�=B�0B�#B� B�B�B�B�B�
B��B��B� B�B�B�)B�GB�=B� B�(B�B�,B�7B�WBǃBȉB��B�MB�B��B		B	B	B	KB	B	#�B	&�B	&�B	'�B	(�B	)�B	)�B	*�B	*�B	.�B	<8B	DlB	DlB	CfB	B]B	AWB	A[B	AXB	CfB	J�B	R�B	W�B	[�B	^B	`B	`B	bB	d&B	f2B	okB	y�B	��B	�GB	�QB	�pB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�.B	�BB	�GB	�SB	�^B	�sB	�|B	�|B	ɃB	ʊB	ˏB	ˑB	ˏB	ΣB	ЮB	ѲB	ҺB	ӿB	��B	ϦB	ΠB	΢B	̘B	ˎB	ʊB	�vB	�dB	�vB	ɄB	ɃB	ˏB	̓B	͚B	ϧB	ЬB	ѴB	ѵB	ΡB	ɂB	�zB	̕B	ϧB	ЮB	ΡB	ɃB	�^B	�B	�B	�B	�B	�B	�B	�(B	�,B	�;B	�LB	�QB	�[B	�dB	�dB	�eB	�cB	�kB	ʉB	ϨB	ҹB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ӾB	ѶB	ѳB	һB	ӿB	��B	��B	��B	��B	��B	��B	� B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�-B	�@B	�LB	�HB	�VB	�^B	�gB	�rB	�tB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B

B
	B
B
B
B
	B
B
	B
B
B
B
B
B
B
B
B
B
B
 B
'B
(B
&B
%B
)B
+B
4B
2B
*B
4B
3B
;B
8B
9B
<G�O�B
EB
#�B
(�B
-�B
3�B
8B
<-B
D_B
I|B
M�B
T�B
Z�B
a
B
e!B
jAB
o^B
syB
x�B
}�B
�B
~�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.51 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451302016080714513020160807145130  AO  ARCAADJP                                                                    20160608021528    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160608021528  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160608021528  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145130  IP                  G�O�G�O�G�O�                