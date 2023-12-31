CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-09-23T19:17:02Z AOML 3.0 creation; 2016-08-07T21:51:21Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20150923191702  20160807145121  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               MA   AO  5287_9017_077                   2C  D   APEX                            6529                            072314                          846 @�q�@H�r1   @�q�βP�@09XbN�d�p��
=1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    MA   B   B   @�ff@�  A   A   A@  A`  A�  A�ffA�ffA�33A�33A�  A�33A�33A�33B��B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<�C=�fC@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV�CX  CY�fC[�fC^  C`  Cb  Cd  Cf  Ch  Cj�Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3D y�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�3Dy��D�  D�I�D���D���D��D�9�D�vfD���D�	�D�I�D�|�D��fD��D�6fD�|�D���D�	�D�C3D�3D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��R@�Q�A(�A((�AH(�Ah(�A�{A�z�A�z�A�G�A�G�A�{A�G�A�G�B��B	��B
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
=B�B�B�B�B�8RB�8RB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�8RB�B�B�B�B�C ��C��C��C��C��C
��C��C��C��C��C��C��C��C��C��C��C ��C"��C$��C&��C(��C*��C,��C.��C0��C2��C4��C6��C8��C:��C<�)C>h�C@��CB��CD��CF��CH��CJ��CL��CN��CP��CR��CT��CV�)CX��CZh�C\h�C^��C`��Cb��Cd��Cf��Ch��Cj�)Cl��Cn��Cp��Cr��Ct��Cv��Cx��Cz��C|��C~��C�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHD =D �>D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D	 �D	��D
 �D
��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D  �D ��D! �D!��D" �D"��D# �D#��D$ �D$��D% �D%��D& �D&��D' �D'��D( �D(��D) �D)��D* �D*��D+ �D+��D, �D,��D- �D-��D. �D.��D/ �D/��D0 �D0��D1 �D1��D2 �D2��D3 �D3��D4 �D4��D5 �D5��D6 �D6��D7 �D7��D8 �D8��D9 �D9��D: �D:��D; �D;��D< �D<��D= �D=��D> �D>��D? �D?��D@ �D@��DA �DA��DB �DB��DC �DC��DD �DD��DE �DE��DF �DF��DG �DG��DH �DH��DI �DI��DJ �DJ��DK �DK��DL �DL��DM �DM��DN �DN��DO �DO��DP �DP��DQ �DQ��DR �DR��DS �DS��DT �DT��DU �DU��DV �DV��DW �DW��DX �DX��DY �DY��DZ �DZ��D[ �D[��D\ �D\��D] �D]��D^ �D^��D_ �D_��D` �D`��Da �Da��Db �Db��Dc �Dc��Dd �Dd��De �De��Df �Df��Dg �Dg��Dh �Dh��Di �Di��Dj �Dj��Dk �Dk��Dl �Dl��Dm �Dm��Dn �Dn��Do �Do��Dp �Dp��Dq �Dq��Dr �Dr��Ds �Ds��Dt �Dt��Dt��Dy�>D�0RD�Y�D��D��D�D�I�D���D��D��D�Y�D��D��D�D�F�DڍD��D��D�S�D�D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�r�A�O�A��A�\A�~�A�r�A�jA�`BA�M�A��A���A�A�7LA�VA٬AׁA֟�A�1A�A�bNA�5?A�A�dZA�E�A�/A��A�1A���Aї�A�+A��A��HA��HA���AЙ�A�=qA���Aϧ�Aϙ�AϓuA�p�A��A��
Aβ-AΓuA��Aͣ�A�~�A�I�A�?}A�;dA�/A�VA���A�~�A�=qA���A���A��/A���A˟�A�z�A�33Aʛ�A�S�A�S�A��yAǧ�A�z�A�/A��TAƕ�A�K�Aš�A�M�A��#A�S�A�$�A��AÙ�A�{AA���A�v�A�=qA�{A��#A���A�VA���A�A�M�A��FA��A���A�7LA���A�Q�A��9A�Q�A��RA���A��A�JA��A��A��hA�bA��A�dZA��`A��wA��PA��`A�dZA{�Ax$�Au��ApbNAlAjE�Af$�Aa+A`�DA[��AV��AU��AT�!AR~�AO+AK�-AI�7AG7LAE%AA��AA�-AA��A@�`A?XA>�A<�/A:��A7|�A7VA4�RA2n�A1|�A0��A0bA,E�A*�DA(bNA'�;A'��A'�A&��A&ZA%�hA%l�A%O�A%A$�9A$�A$~�A$(�A#oA!�
A!/A �9A��A�AbNA��AS�AoA�9AM�A��AC�A=qA��AA�HA1'AC�A��Al�A��A33A5?A�#A��A�AK�AVAl�A\)AS�A�A��A��A
(�A	x�AȴA~�Al�AA/AVA�
AoA �D@�C�@��@��H@��H@���@�@��@���@�33@��
@��@�hs@��j@��^@�p�@��@�"�@���@�^5@��h@�p�@�x�@�hs@�G�@���@��j@�A�@�C�@�ȴ@�ff@�$�@��T@��@�V@�@�+@���@�+@�$�@�
=@��@@�=q@��-@�J@�!@��^@���@�1'@�\)@�l�@�@�u@�I�@�\)@�@��/@�S�@�`B@���@�|�@�~�@�x�@߶F@��@���@ܛ�@�G�@��@���@ۮ@ڸR@���@ڇ+@ٙ�@ؼj@��@ָR@֏\@ָR@�O�@��@�o@���@��@�1@�@Η�@�V@�r�@���@��@��y@�^5@�~�@��m@�ff@�ff@���@ȴ9@�`B@�?}@�  @���@�K�@���@ǶF@Ƨ�@��@�&�@ļj@��@ũ�@�33@�ȴ@���@��@�V@��9@�z�@���@��!@�^5@���@��-@���@���@�X@��D@�r�@��@�\)@�@�
=@��y@��#@��@��@���@���@��@�ƨ@�33@�o@���@��+@�E�@�@��@�X@��j@��m@��m@��@��F@�t�@�|�@��@�|�@�C�@��@�^5@�n�@�n�@��@��@��j@���@�1'@���@�ƨ@���@�l�@�;d@���@��@�V@��@��^@��@���@���@���@�Ĝ@���@�1'@��;@�S�@��H@�V@�-@��T@���@���@�1@���@��P@�\)@�"�@���@��+@�$�@��#@�hs@��9@�1'@��
@�t�@�|�@�\)@�C�@��@��R@�~�@�^5@�-@��@���@���@�/@���@���@�1'@���@�l�@�K�@�+@�@��@��+@�E�@���@��@��@���@���@���@��7@�`B@��@��D@��@���@���@�;d@���@�E�@��@��@���@��7@�X@�%@��@�%@�bN@��w@�dZ@���@���@��+@�E�@��@��h@�7L@���@���@��9@�j@�ƨ@�;d@�o@��y@��+@�M�@��@��@�I�@��+@�bN@}��@v�R@kdZ@a�^@Y7L@O|�@GK�@?��@8Ĝ@2��@+S�@&ff@!�#@O�@G�@�j@�@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�r�A�O�A��A�\A�~�A�r�A�jA�`BA�M�A��A���A�A�7LA�VA٬AׁA֟�A�1A�A�bNA�5?A�A�dZA�E�A�/A��A�1A���Aї�A�+A��A��HA��HA���AЙ�A�=qA���Aϧ�Aϙ�AϓuA�p�A��A��
Aβ-AΓuA��Aͣ�A�~�A�I�A�?}A�;dA�/A�VA���A�~�A�=qA���A���A��/A���A˟�A�z�A�33Aʛ�A�S�A�S�A��yAǧ�A�z�A�/A��TAƕ�A�K�Aš�A�M�A��#A�S�A�$�A��AÙ�A�{AA���A�v�A�=qA�{A��#A���A�VA���A�A�M�A��FA��A���A�7LA���A�Q�A��9A�Q�A��RA���A��A�JA��A��A��hA�bA��A�dZA��`A��wA��PA��`A�dZA{�Ax$�Au��ApbNAlAjE�Af$�Aa+A`�DA[��AV��AU��AT�!AR~�AO+AK�-AI�7AG7LAE%AA��AA�-AA��A@�`A?XA>�A<�/A:��A7|�A7VA4�RA2n�A1|�A0��A0bA,E�A*�DA(bNA'�;A'��A'�A&��A&ZA%�hA%l�A%O�A%A$�9A$�A$~�A$(�A#oA!�
A!/A �9A��A�AbNA��AS�AoA�9AM�A��AC�A=qA��AA�HA1'AC�A��Al�A��A33A5?A�#A��A�AK�AVAl�A\)AS�A�A��A��A
(�A	x�AȴA~�Al�AA/AVA�
AoA �D@�C�@��@��H@��H@���@�@��@���@�33@��
@��@�hs@��j@��^@�p�@��@�"�@���@�^5@��h@�p�@�x�@�hs@�G�@���@��j@�A�@�C�@�ȴ@�ff@�$�@��T@��@�V@�@�+@���@�+@�$�@�
=@��@@�=q@��-@�J@�!@��^@���@�1'@�\)@�l�@�@�u@�I�@�\)@�@��/@�S�@�`B@���@�|�@�~�@�x�@߶F@��@���@ܛ�@�G�@��@���@ۮ@ڸR@���@ڇ+@ٙ�@ؼj@��@ָR@֏\@ָR@�O�@��@�o@���@��@�1@�@Η�@�V@�r�@���@��@��y@�^5@�~�@��m@�ff@�ff@���@ȴ9@�`B@�?}@�  @���@�K�@���@ǶF@Ƨ�@��@�&�@ļj@��@ũ�@�33@�ȴ@���@��@�V@��9@�z�@���@��!@�^5@���@��-@���@���@�X@��D@�r�@��@�\)@�@�
=@��y@��#@��@��@���@���@��@�ƨ@�33@�o@���@��+@�E�@�@��@�X@��j@��m@��m@��@��F@�t�@�|�@��@�|�@�C�@��@�^5@�n�@�n�@��@��@��j@���@�1'@���@�ƨ@���@�l�@�;d@���@��@�V@��@��^@��@���@���@���@�Ĝ@���@�1'@��;@�S�@��H@�V@�-@��T@���@���@�1@���@��P@�\)@�"�@���@��+@�$�@��#@�hs@��9@�1'@��
@�t�@�|�@�\)@�C�@��@��R@�~�@�^5@�-@��@���@���@�/@���@���@�1'@���@�l�@�K�@�+@�@��@��+@�E�@���@��@��@���@���@���@��7@�`B@��@��D@��@���@���@�;d@���@�E�@��@��@���@��7@�X@�%@��@�%@�bN@��w@�dZ@���@���@��+@�E�@��@��h@�7L@���@���@��9@�j@�ƨ@�;d@�o@��y@��+@�M�@��G�O�@�I�@��+@�bN@}��@v�R@kdZ@a�^@Y7L@O|�@GK�@?��@8Ĝ@2��@+S�@&ff@!�#@O�@G�@�j@�@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
VB
S�B
P�B
N�B
N�B
N�B
N�B
N�B
O�B
G�B
9XB
$�B
�B
1B	�B	�B	�yB	�mB	�fB	�fB	�ZB	�ZB	�ZB	�ZB	�ZB	�ZB	�ZB	�`B	�`B	�fB	�`B	�`B	�`B	�`B	�ZB	�`B	�ZB	�ZB	�`B	�fB	�sB	�B	�B	�B	�B	��B
B
	7B
VB
VB
VB
\B
hB
oB
&�B
6FB
B�B
C�B
O�B
[#B
gmB
w�B
�7B
�B
�wB
��B>wBI�BQ�B^5Bk�Bx�B�B��B�B�dB��B�B�HB�B��B\B�B"�B1'BM�BXBZB]/B_;BaHBm�Bl�BdZBaHBcTBe`BffBffBaHBZBR�BM�BC�B8RB)�B  B�jBjBoB
�)B
��B
�B
ffB
�B	�mB	��B	��B	��B	�=B	|�B	e`B	K�B	D�B	0!B	�B	�B	bB	1B	B��B�B�B�ZB�#B�B�B��B��B��B��BĜBB�wB�wB�jB�^B�RB�9B�B��B��B��B��B��B��B��B�B�B�B�B��BǮBɺB��B��B�B�#B�5B�BB�NB�ZB�fB�sB�sB�sB�yB�yB�sB�B�B�B�B��B��B��B��B��B�B�B�yB�B�B�B�B�`B�B��B��B�B�B�B�B�yB�5B�B�B��B�B�NB��B��B	B	1B		7B	JB	�B	$�B	(�B	$�B	)�B	5?B	8RB	9XB	>wB	<jB	@�B	J�B	XB	^5B	cTB	aHB	bNB	gmB	jB	k�B	k�B	r�B	s�B	t�B	v�B	x�B	y�B	z�B	}�B	�B	�1B	�7B	�+B	� B	�B	�+B	�1B	�+B	�DB	�DB	�hB	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	��B	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�dB	�LB	�XB	�LB	�dB	B	ĜB	��B	�jB	ÖB	ɺB	��B	��B	ȴB	ŢB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�B	�B	�#B	�5B	�5B	�;B	�BB	�HB	�TB	�`B	�mB	�fB	�`B	�ZB	�`B	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�yB	�yB	�yB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
	7B
	7B
1B
	7B
	7B
	7B
	7B
DB
VB
hB
hB
hB
hB
hB
hB
hB
hB
bB
\B
bB
hB
hB
oB
oB
uB
uB
uB
uB
{B
{B
{B
�B
�B
�B
�B
 �B
'�B
6FB
=qB
B�B
I�B
O�B
T�B
ZB
_;B
e`B
iyB
m�B
q�B
t�B
x�B
|�B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
U�B
S�B
P�B
N�B
N�B
N�B
N�B
N�B
O�B
G�B
99B
$�B
�B
B	��B	�gB	�[B	�PB	�JB	�HB	�9B	�:B	�;B	�;B	�=B	�;B	�9B	�BB	�BB	�GB	�BB	�?B	�CB	�AB	�:B	�@B	�>B	�;B	�CB	�HB	�TB	�gB	�rB	�qB	�~B	��B
�B
	B
3B
8B
6B
<B
EB
KB
&�B
6$B
BlB
CqB
O�B
[B
gJB
w�B
�B
��B
�OB
��B>PBI�BQ�B^Bk\Bx�B��B��B��B�9B��B��B�B�fB��B/B~B"�B0�BM�BW�BY�B]B_BaBmfBlZBd.BaBc&Be1Bf7Bf8BaBY�BR�BM�BCiB8&B)�B��B�:BjPB@B
��B
�cB
��B
f7B
[B	�@B	��B	�^B	��B	�B	|�B	e9B	K�B	DtB	/�B	�B	eB	>B	B	�B��B�B�bB�4B��B��B��B��BϹBͬBʜB�yB�jB�RB�QB�DB�9B�+B�B��B��B��B��B��B��B��B��B��B��B��B��B�`BǇBɓB̤B��B��B��B�	B�B�$B�/B�<B�HB�GB�IB�OB�NB�HB�dB�wB�B�B��B��B��B��B��B�cB�YB�MB�TB�SB�dB�ZB�2B�xB��B��B�B�}B�B�lB�LB�B��B��B��B��B�#B�B��B	�B	B		
B	B	WB	$�B	(�B	$�B	)�B	5B	8#B	9'B	>EB	<7B	@QB	J�B	W�B	^B	c!B	aB	bB	g:B	jKB	kTB	kRB	r|B	s�B	t�B	v�B	x�B	y�B	z�B	}�B	��B	��B	�B	��B	�B	��B	��B	��B	��B	�B	�B	�3B	�oB	�}B	�B	��B	�vB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�vB	�pB	�jB	�wB	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�-B	�B	�B	�B	�-B	�WB	�eB	�NB	�2B	�\B	ɃB	ΡB	ˎB	�{B	�jB	�xB	ЬB	ѵB	ΡB	͛B	ΠB	͚B	̔B	͙B	ШB	ҺB	ҸB	ӾB	ҺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�2B	�,B	�'B	� B	�&B	�4B	�:B	�@B	�EB	�CB	�CB	�DB	�DB	�DB	�LB	�KB	�IB	�LB	�LB	�KB	�AB	�?B	�?B	�?B	�8B	�AB	�=B	�QB	�PB	�VB	�]B	�\B	�_B	�jB	�}B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�{B	�pB	�pB	�iB	�oB	�nB	�oB	�|B	�{B	�zB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	B
B
+B
+B
-B
-B
+B
-B
-B
,B
%B
 B
&B
,B
-B
1B
2B
:B
9B
:B
8B
=B
>B
?G�O�B
JB
DB
lB
 �B
'�B
6B
=4B
BRB
I|B
O�B
T�B
Y�B
^�B
e!B
i;B
mRB
qmB
t|B
x�B
|�B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.51 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451212016080714512120160807145121  AO  ARCAADJP                                                                    20150923191702    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150923191702  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20150923191702  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145121  IP                  G�O�G�O�G�O�                