CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2016-05-18T02:15:24Z AOML 3.0 creation; 2016-08-07T21:51:29Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20160518021524  20160825183417  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               zA   AO  5287_9017_122                   2C  D   APEX                            6529                            072314                          846 @׬�E�G�1   @׬���m<@0p��
=q�dץ�S��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    zA   B   B   @���@�  A   A   A@  A`  A�  A�33A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C�C	�fC  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CK�fCN  CP  CR  CT  CV  CX�CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DCy�DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt` Dx�3D��D�FfD�� D���D�3D�<�D�|�D��3D�3D�<�D�|�DǬ�D�3D�S3Dډ�D��fD��D�L�D�p D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@�Q�A(�A((�AH(�Ah(�A�{A�G�A�{A�{A�{A�{A�{A�{B
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
=B�B�B�B�B�B�B�B�B�8RB�B�B���B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C ��C��C��C��C�)C
h�C��C��C��C��C��C��C��C��C��C��C ��C"��C$��C&��C(��C*��C,��C.��C0��C2��C4��C6��C8��C:��C<��C>��C@��CB��CD��CF��CH��CJ��CLh�CN��CP��CR��CT��CV��CX�)CZ��C\��C^��C`��Cb��Cd��Cf��Ch��Cj��Cl��Cn��Cp��Cr��Ct��Cv��Cx��Cz��C|��C~��C�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�4{C�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHD  �D ��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D	 �D	��D
 �D
��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D  �D ��D! �D!��D" �D"��D# �D#��D$ �D$��D% �D%��D& �D&��D' �D'��D( �D(��D) �D)��D* �D*��D+ �D+��D, �D,��D- �D-��D. �D.��D/ �D/��D0 �D0��D1 �D1��D2 �D2��D3 �D3��D4 �D4��D5 �D5��D6 �D6��D7 �D7��D8 �D8��D9 �D9��D: �D:��D; �D;��D< �D<��D= �D=��D> �D>��D? �D?��D@ �D@��DA �DA��DB �DB��DC �DC�>DD �DD��DE �DE��DF �DF��DG �DG��DH �DH��DI �DI��DJ �DJ��DK �DK��DL �DL��DM �DM��DN �DN��DO �DO��DP �DP��DQ �DQ��DR �DR��DS �DS��DT �DT��DU �DU��DV �DV��DW �DW��DX �DX��DY �DY��DZ �DZ��D[ �D[��D\ �D\��D] �D]��D^ �D^��D_ �D_��D` �D`��Da �Da��Db �Db��Dc �Dc��Dd �Dd��De �De��Df �Df��Dg �Dg��Dh �Dh��Di �Di��Dj �Dj��Dk �Dk��Dl �Dl��Dm �Dm��Dn �Dn��Do �Do��Dp �Dp��Dq �Dq��Dr �Dr��Ds �Ds��Dt �Dt��Dx��D�D�V�D��RD��D��D�MD��D��D��D�MD��DǽD�#�D�c�Dڙ�D��D�D�]D�RD��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A���AԶFAԬA԰!Aԥ�Aԥ�Aԥ�Aԥ�Aԥ�Aԥ�Aԣ�Aԡ�Aԡ�Aԟ�Aԟ�Aԗ�A�x�A�33A�bA��`Aӥ�Aӝ�Aӕ�AӑhA�~�A�1A���AҶFAҩ�A�K�A��yA�"�A�jA�A���A��A�/A�C�A��`A̓A���A� �Aɏ\A�/A�dZAƲ-A��A�$�A��A�ĜA�33A�A�A�A�A���A��A��TA��^A��HA�E�A��`A��A���A�VA���A�p�A�G�A�z�A���A�{A�ƨA��`A��A�  A�G�A�oA���A���A��RA���A���A�hsA�/A���A�p�A�;dA���A� �A�=qA�bNA���A�7LA���A���A�?}A���A���A�|�A���A��;A�VA��DA��9Ax�A|�yA{+AydZAxA�Awx�Au�-Asl�Apr�Al�AlM�AlA�Al �Al  Ak�;Akt�Ah�\Af�AfVAdz�Ab�DAaG�A_�7A^ffA]?}A\�AZ�/AY��AY/AXbNAWG�AV�AT��ARn�AP  AI�-AFr�AEAEdZABQ�A@M�A>ffA<jA:�!A:jA:VA9��A7�hA6E�A5�mA3�TA1�#A.�9A,��A+7LA*��A*(�A)��A(n�A&�A%�TA%��A$�A#�A"�!A ��AQ�A��A�HAl�A��A=qA��AG�A �A&�A-A�/A�A�AbNA\)A&�A�uA  A�-A�+AXAĜA�^A
��A	��A�AdZA��A�A��A�yA/A��A�RAȴA�A-AbA�^A�A|�At�Al�At�At�A�A=qA�A9XA{AE�AbAt�A �A �@�-@�`B@�V@��@�I�@��R@��@��;@��`@���@�`B@�ƨ@�;d@��@���@�7L@蛦@��m@�
=@�{@�7L@�S�@�=q@���@��@�ƨ@�
=@�ȴ@�5?@��`@�Z@���@۝�@ە�@�l�@ڏ\@�`B@��`@أ�@؋D@�r�@�Z@�1'@��@�dZ@�o@��@���@�(�@Ӿw@�+@�v�@�^5@�E�@�5?@�-@�@��@�r�@�Z@�9X@Ͼw@�;d@�V@��T@�`B@̴9@�Q�@ˍP@�v�@�@ɺ^@��@��@ǅ@�K�@�C�@�33@�
=@��@ƸR@�J@őh@�/@Ĭ@��
@Õ�@Õ�@��@�-@���@��-@���@�?}@���@�1'@��@��+@�-@�J@��@�/@���@�z�@�1'@��F@�|�@���@���@���@��R@���@�5?@�X@��@��/@��@��@�33@���@��\@��@�hs@���@���@�Z@� �@��m@�;d@�ȴ@�^5@�$�@�@��@��@��^@�p�@���@��u@�Q�@�A�@���@��F@��F@��F@�|�@�@���@�ff@��^@���@��/@���@�Ĝ@��9@��D@�r�@�  @�ƨ@��F@��P@�33@���@�-@��-@�O�@�V@���@�Q�@��w@�\)@�"�@�@��@��@��R@��!@���@���@�n�@�$�@�@���@�O�@��@��@��@�Ĝ@���@�r�@�ƨ@�t�@�33@���@���@�v�@��#@�hs@�%@��j@��9@��@��@��u@�z�@�Z@�A�@�(�@��m@�K�@�o@��y@��@�ȴ@���@�$�@��^@�G�@�Ĝ@�j@�Z@�Z@�I�@���@��F@�dZ@��@��!@�n�@��@�p�@��@��@�%@��`@���@��D@��m@�ƨ@��@��@�$�@��T@��^@�x�@�&�@�z�@��F@��@���@�~�@�ff@�-@��T@���@��-@���@�`B@�O�@�/@��/@���@��@�bN@��@{�@pA�@fff@_l�@YX@N{@F5?@@��@5�h@0�@*�@#�m@�@=q@�y@t�@  @C�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A���A���AԶFAԬA԰!Aԥ�Aԥ�Aԥ�Aԥ�Aԥ�Aԥ�Aԣ�Aԡ�Aԡ�Aԟ�Aԟ�Aԗ�A�x�A�33A�bA��`Aӥ�Aӝ�Aӕ�AӑhA�~�A�1A���AҶFAҩ�A�K�A��yA�"�A�jA�A���A��A�/A�C�A��`A̓A���A� �Aɏ\A�/A�dZAƲ-A��A�$�A��A�ĜA�33A�A�A�A�A���A��A��TA��^A��HA�E�A��`A��A���A�VA���A�p�A�G�A�z�A���A�{A�ƨA��`A��A�  A�G�A�oA���A���A��RA���A���A�hsA�/A���A�p�A�;dA���A� �A�=qA�bNA���A�7LA���A���A�?}A���A���A�|�A���A��;A�VA��DA��9Ax�A|�yA{+AydZAxA�Awx�Au�-Asl�Apr�Al�AlM�AlA�Al �Al  Ak�;Akt�Ah�\Af�AfVAdz�Ab�DAaG�A_�7A^ffA]?}A\�AZ�/AY��AY/AXbNAWG�AV�AT��ARn�AP  AI�-AFr�AEAEdZABQ�A@M�A>ffA<jA:�!A:jA:VA9��A7�hA6E�A5�mA3�TA1�#A.�9A,��A+7LA*��A*(�A)��A(n�A&�A%�TA%��A$�A#�A"�!A ��AQ�A��A�HAl�A��A=qA��AG�A �A&�A-A�/A�A�AbNA\)A&�A�uA  A�-A�+AXAĜA�^A
��A	��A�AdZA��A�A��A�yA/A��A�RAȴA�A-AbA�^A�A|�At�Al�At�At�A�A=qA�A9XA{AE�AbAt�A �A �@�-@�`B@�V@��@�I�@��R@��@��;@��`@���@�`B@�ƨ@�;d@��@���@�7L@蛦@��m@�
=@�{@�7L@�S�@�=q@���@��@�ƨ@�
=@�ȴ@�5?@��`@�Z@���@۝�@ە�@�l�@ڏ\@�`B@��`@أ�@؋D@�r�@�Z@�1'@��@�dZ@�o@��@���@�(�@Ӿw@�+@�v�@�^5@�E�@�5?@�-@�@��@�r�@�Z@�9X@Ͼw@�;d@�V@��T@�`B@̴9@�Q�@ˍP@�v�@�@ɺ^@��@��@ǅ@�K�@�C�@�33@�
=@��@ƸR@�J@őh@�/@Ĭ@��
@Õ�@Õ�@��@�-@���@��-@���@�?}@���@�1'@��@��+@�-@�J@��@�/@���@�z�@�1'@��F@�|�@���@���@���@��R@���@�5?@�X@��@��/@��@��@�33@���@��\@��@�hs@���@���@�Z@� �@��m@�;d@�ȴ@�^5@�$�@�@��@��@��^@�p�@���@��u@�Q�@�A�@���@��F@��F@��F@�|�@�@���@�ff@��^@���@��/@���@�Ĝ@��9@��D@�r�@�  @�ƨ@��F@��P@�33@���@�-@��-@�O�@�V@���@�Q�@��w@�\)@�"�@�@��@��@��R@��!@���@���@�n�@�$�@�@���@�O�@��@��@��@�Ĝ@���@�r�@�ƨ@�t�@�33@���@���@�v�@��#@�hs@�%@��j@��9@��@��@��u@�z�@�Z@�A�@�(�@��m@�K�@�o@��y@��@�ȴ@���@�$�@��^@�G�@�Ĝ@�j@�Z@�Z@�I�@���@��F@�dZ@��@��!@�n�@��@�p�@��@��@�%@��`@���@��D@��m@�ƨ@��@��@�$�@��T@��^@�x�@�&�@�z�@��F@��@���@�~�@�ff@�-@��T@���@��-@���@�`B@�O�@�/@��/G�O�@��@�bN@��@{�@pA�@fff@_l�@YX@N{@F5?@@��@5�h@0�@*�@#�m@�@=q@�y@t�@  @C�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�;B
�HB
�ZB
�sB
�sB
�yB
�sB
�yB
��B
��BBBPB{B�B'�B.B/B0!B<jBG�BJ�BP�Be`B��B\B�B+BL�B[#Bl�Br�Bw�B�B�{B�{B��B�B�9B�FB�RBĜB��B�B�5B�HB�HB�#B��Bw�B_;BW
BO�BG�B?}B8RB1'B'�B"�B�B+B��B�B�B�TB�HB�/B�B�/B��B��B�^B�LB��B��B�uB}�BgmBO�BB�BhB
�}B
��B
z�B
l�B
^5B
=qB
'�B
�B
uB
hB
PB
B	��B	�mB	�B	��B	��B	��B	��B	��B	ƨB	�^B	�B	�B	��B	��B	�bB	�1B	�B	y�B	t�B	n�B	gmB	dZB	`BB	YB	S�B	J�B	;dB	)�B	hB	B��B��B�B�sB�NB�)B�B�B�
B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BɺB��B��B��B��B��B��B��B��B��B�B�)B�;B�fB�B�B�B�B�B��B��B��B��B��B��B��B	B	B	B	+B		7B	1B	1B	DB	PB	�B	�B	$�B	1'B	D�B	I�B	I�B	K�B	L�B	P�B	S�B	VB	VB	VB	VB	T�B	VB	W
B	\)B	e`B	ffB	n�B	l�B	gmB	gmB	cTB	aHB	aHB	dZB	jB	y�B	x�B	o�B	gmB	`BB	]/B	VB	P�B	N�B	M�B	L�B	N�B	O�B	Q�B	R�B	S�B	S�B	XB	[#B	^5B	`BB	aHB	e`B	ffB	ffB	gmB	gmB	hsB	iyB	iyB	jB	q�B	t�B	v�B	v�B	w�B	w�B	x�B	y�B	z�B	|�B	}�B	� B	�B	�B	�B	�B	�1B	�1B	�1B	�1B	�1B	�1B	�DB	�VB	�VB	�VB	�\B	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�!B	�!B	�'B	�-B	�9B	�FB	�XB	�dB	�dB	�dB	�wB	�}B	��B	��B	B	ÖB	ŢB	ŢB	ŢB	ŢB	ŢB	ƨB	ȴB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�#B	�#B	�/B	�/B	�/B	�/B	�/B	�5B	�;B	�;B	�HB	�TB	�TB	�ZB	�ZB	�ZB	�ZB	�ZB	�`B	�fB	�fB	�fB	�fB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
1B
1B
1B
	7B
	7B

=B

=B

=B
DB
JB
JB
JB
JB
JB
JB
PB
PB
JB
PB
VB
\B
VB
VB
VB
VB
VB
PB
JB
JB
PB
PB
VB
VB
VB
VB
\B
\B
\B
\B
hB
hB
�B
#�B
.B
7LB
<jB
@�B
D�B
K�B
O�B
R�B
ZB
^5B
dZB
k�B
q�B
s�B
v�B
z�B
~�B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�&B
�3B
�MB
�JB
�TB
�MB
�RB
��B
��B �B�B*BSB�B'�B-�B.�B/�B<BBG�BJ�BP�Be9BβB/BpB*�BL�BZ�BlcBr�Bw�B��B�NB�MB�^B��B�B�B�,B�sBϴB��B�B� B�B��B��Bw�B_BV�BO�BG�B?SB8#B0�B'�B"�BnB�B��B�vB�bB�&B�B��B��B��BѼB�WB�0B�B��B�uB�DB}�Bg?BO�BBbB6B
�NB
��B
z�B
l_B
^B
=BB
'�B
sB
GB
<B
"B
�B	��B	�@B	��B	��B	иB	ϱB	ͨB	˘B	�{B	�3B	��B	��B	��B	�bB	�9B	�B	��B	y�B	t�B	noB	gEB	d.B	`B	X�B	S�B	J�B	;<B	)�B	AB	�B��B��B�}B�LB�(B�B��B��B��B��B��B��B��B��BϸB��BнBͭBͫBͫBαB��BͪBʘBɒBлB��BмBͪB��B��B��B��B��B��B� B�B�<B�TB�YB�yB�B�B��B��B��B��B��B��B��B	 �B	�B	�B	�B		
B	B	B	B	"B	cB	|B	$�B	0�B	DlB	I�B	I�B	K�B	L�B	P�B	S�B	U�B	U�B	U�B	U�B	T�B	U�B	V�B	[�B	e-B	f4B	neB	lXB	g:B	g;B	c B	aB	aB	d*B	jLB	y�B	x�B	oiB	g8B	`B	\�B	U�B	P�B	N�B	M�B	L�B	N�B	O�B	Q�B	R�B	S�B	S�B	W�B	Z�B	^B	`B	aB	e-B	f0B	f0B	g8B	g9B	h?B	iEB	iEB	jIB	qtB	t�B	v�B	v�B	w�B	w�B	x�B	y�B	z�B	|�B	}�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�B	�&B	�8B	�FB	�IB	�KB	�HB	�PB	�VB	�cB	�cB	�hB	�nB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�.B	�-B	�-B	�>B	�BB	�JB	�QB	�WB	�\B	�jB	�iB	�kB	�iB	�hB	�pB	�|B	ɀB	�B	ɂB	ˎB	̔B	̔B	̒B	̕B	͚B	ΠB	ΟB	ϧB	ϤB	ϦB	ЬB	ѲB	ҸB	��B	ӾB	ӿB	ӿB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	��B	�B	�B	�B	�B	�B	�B	� B	� B	�%B	�*B	�,B	�+B	�,B	�2B	�?B	�DB	�IB	�JB	�KB	�RB	�]B	�cB	�cB	�kB	�lB	�iB	�hB	�iB	�hB	�iB	�qB	�nB	�nB	�rB	�zB	�{B	�{B	�|B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B

B

 B

B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
 B
 B
!G�O�B
-B
uB
#�B
-�B
7B
<,B
@DB
D^B
K�B
O�B
R�B
Y�B
]�B
dB
kEB
qjB
sxB
v�B
z�B
~�B
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.51 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451292016080714512920160807145129  AO  ARCAADJP                                                                    20160518021524    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160518021524  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160518021524  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145129  IP                  G�O�G�O�G�O�                