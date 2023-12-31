CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-02-26T22:14:29Z AOML 3.0 creation; 2016-08-07T21:51:13Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20150226221429  20160807145113  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  5287_9017_027                   2C  D   APEX                            6529                            072314                          846 @�/�3���1   @�/����@2bM���dl�t�j1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @�  @�  A   A   A>ffA^ffA�  A�  A�  A�  A�  A�33A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
�C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8�C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr�Ct�Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"y�D#  D#� D$  D$�fD%  D%� D&  D&� D'  D'� D(  D(� D)  D)�fD*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dts3Dy�3D��D�C3D��3D���D��D�0 D�s3D�� D��D�<�D�� Dǩ�D�fD�S3D�P D��D�  D�C3D�y�D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�Q�@�Q�A(�A((�AF�\Af�\A�{A�{A�{A�{A�{A�G�A�{A�{B
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
=B�B�B�B�B�B�B�B�B�B�B�8RB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C ��C��C��C��C��C
�)C��C��C��C��C��C��C��C��C��C��C ��C"��C$��C&��C(��C*��C,��C.��C0��C2��C4��C6��C8�)C:��C<��C>��C@��CB��CD��CF��CH��CJ��CL��CN��CP��CR��CT��CV��CX��CZ��C\��C^��C`��Cb��Cd��Cf��Ch��Cj��Cl��Cn��Cp��Cr�)Ct�)Cv��Cx��Cz��C|��C~��C�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�NC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�4{C�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHD  �D ��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D	 �D	��D
 �D
��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D  �D ��D! �D!��D" �D"�>D# �D#��D$ �D$�
D% �D%��D& �D&��D' �D'��D( �D(��D) �D)�
D* �D*��D+ �D+��D, �D,��D- �D-��D. �D.��D/ �D/��D0 �D0��D1 �D1��D2 �D2��D3 �D3��D4 �D4��D5 �D5��D6 �D6��D7 �D7��D8 �D8��D9 �D9��D: �D:��D; �D;��D< �D<��D= �D=��D> �D>��D? �D?��D@ �D@��DA �DA��DB �DB��DC �DC��DD �DD��DE �DE��DF �DF��DG �DG��DH �DH��DI �DI��DJ �DJ��DK �DK��DL �DL��DM �DM��DN �DN��DO �DO��DP �DP��DQ �DQ��DR �DR��DS �DS��DT �DT��DU �DU��DV �DV��DW �DW��DX �DX��DY �DY��DZ �DZ��D[ �D[��D\ �D\��D] �D]��D^ �D^��D_ �D_��D` �D`��Da �Da��Db �Db��Dc �Dc��Dd �Dd��De �De��Df �Df��Dg �Dg��Dh �Dh��Di �Di��Dj �Dj��Dk �Dk��Dl �Dl��Dm �Dm��Dn �Dn��Do �Do��Dp �Dp��Dq �Dq��Dr �Dr��Ds �Ds��Dt �Dt��Dy��D�D�S�D���D��D�D�@RD���D��RD�)�D�MD��RDǹ�D��D�c�D�`RD��D�RD�S�D��D��R11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�n�A�p�A�p�A�p�A�p�A�t�A�n�A�ffA�^5A�bNA�^5A�\)A�\)A�^5A�ZA�S�A�S�A�S�A�O�A�Q�A�Q�A�Q�A�O�A�Q�A�Q�A�S�A�S�A�Q�A�Q�A�Q�A�Q�A�S�A�S�A�S�A�S�A�Q�A�S�A�S�A�Q�A�Q�A�Q�A�S�A�S�A�Q�A�O�A�O�A�M�A�?}A���A�A���A�n�A��A��HAǼjAǗ�AƍPAżjA�r�A�VA���A�ȴA��A��A��+A�S�A�v�A�"�A��uA���A��A�I�A���A��A���A���A�|�A�~�A�dZA�XA��+A�A���A��uA�\)A�33A�ƨA��mA��A��A��`A�/A� �A��A�|�A�v�A�VA�{A�&�A�\)A���A�|�A��wA���A��hA�bNA���A�+A�K�A�G�A��;A�$�A��PA��7A�r�A�E�A�M�A���A�1'A~��Ay\)Au�TAk33Ab�AW�hAP�9AN�HAM�TAK�TAJ�AG?}AD��AB�AA�FA@��A@=qA=�A:�A8n�A6�jA5�A4M�A1p�A0  A-�TA,�RA+�;A*��A)A&9XA$��A#x�A"��A!�mA!&�A
=A$�AĜA�-A�9Az�A-A�A��A��An�A �A`BA+A��AjA�7A
=A�yA~�A/A�A�A9XA�A��AhsA+A~�A��A�AĜA(�A�TA��AO�A
�`A
�!A
��A
ffA
=qA
5?A
  A	��A�`AZAQ�AA��A��AdZAjA�Al�A��A�\AbNAA�9AC�A v�@�ƨ@�C�@���@�Ĝ@��@���@�5?@�`B@� �@�v�@�O�@�33@�v�@�-@�-@���@�V@��/@�j@�  @��@��@�
=@�\@�$�@��-@��`@�1@�|�@�R@�=q@���@��@�(�@��@�+@�ff@�J@�`B@�1'@�1@�  @��
@�@�;d@��y@⟾@��@�O�@�/@�&�@�V@�9@�|�@�o@޸R@�v�@�M�@���@ܛ�@��
@���@��@�"�@�@�~�@�-@��@�z�@���@ם�@�
=@���@պ^@�-@�{@�@�X@��@�1'@ӍP@�33@ҧ�@�M�@��@���@�X@�1@υ@�o@���@Ώ\@�$�@���@�p�@��@̣�@�1'@�ƨ@�t�@�"�@���@�ȴ@�v�@�5?@���@�`B@��@��/@�r�@�I�@�9X@��m@ǥ�@ǝ�@�33@���@�M�@���@�@ŉ7@�&�@��@�Ĝ@�z�@��;@��@�5?@�x�@���@�Z@���@��@���@�ff@�=q@�-@�{@��^@���@�x�@�O�@�/@��@�z�@��@��H@�x�@�%@���@���@�  @�\)@�o@��y@���@�=q@��@���@���@�X@�7L@�Ĝ@�+@�
=@�5?@�@�G�@��@��@��@�1'@��H@�v�@�=q@�{@��#@���@�`B@�V@��9@�  @�|�@�dZ@�"�@�ȴ@��\@�V@�$�@��@��-@�p�@�?}@��j@�Q�@��@�
=@�;d@�;d@��!@���@��!@�5?@���@���@��@�X@�&�@���@�Ĝ@�9X@�;d@���@��y@�^5@��#@��@�V@�A�@���@�|�@�t�@��@�l�@�dZ@��@�V@���@��@��@��@���@�X@��@���@�Q�@�1'@�1@��;@��P@�;d@�@��R@���@�ff@�5?@��@��#@���@�@���@�hs@���@��@���@��@���@�Z@��@��
@�ƨ@��F@��P@�|�@�o@�ȴ@�~�@�^5@�=q@��@���@�G�@�^5@���@��D@��
@~@s"�@fV@a��@Y��@R=q@J~�@A��@:�@3ƨ@,I�@(�@#��@�P@-@V@hs11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A�n�A�p�A�p�A�p�A�p�A�t�A�n�A�ffA�^5A�bNA�^5A�\)A�\)A�^5A�ZA�S�A�S�A�S�A�O�A�Q�A�Q�A�Q�A�O�A�Q�A�Q�A�S�A�S�A�Q�A�Q�A�Q�A�Q�A�S�A�S�A�S�A�S�A�Q�A�S�A�S�A�Q�A�Q�A�Q�A�S�A�S�A�Q�A�O�A�O�A�M�A�?}A���A�A���A�n�A��A��HAǼjAǗ�AƍPAżjA�r�A�VA���A�ȴA��A��A��+A�S�A�v�A�"�A��uA���A��A�I�A���A��A���A���A�|�A�~�A�dZA�XA��+A�A���A��uA�\)A�33A�ƨA��mA��A��A��`A�/A� �A��A�|�A�v�A�VA�{A�&�A�\)A���A�|�A��wA���A��hA�bNA���A�+A�K�A�G�A��;A�$�A��PA��7A�r�A�E�A�M�A���A�1'A~��Ay\)Au�TAk33Ab�AW�hAP�9AN�HAM�TAK�TAJ�AG?}AD��AB�AA�FA@��A@=qA=�A:�A8n�A6�jA5�A4M�A1p�A0  A-�TA,�RA+�;A*��A)A&9XA$��A#x�A"��A!�mA!&�A
=A$�AĜA�-A�9Az�A-A�A��A��An�A �A`BA+A��AjA�7A
=A�yA~�A/A�A�A9XA�A��AhsA+A~�A��A�AĜA(�A�TA��AO�A
�`A
�!A
��A
ffA
=qA
5?A
  A	��A�`AZAQ�AA��A��AdZAjA�Al�A��A�\AbNAA�9AC�A v�@�ƨ@�C�@���@�Ĝ@��@���@�5?@�`B@� �@�v�@�O�@�33@�v�@�-@�-@���@�V@��/@�j@�  @��@��@�
=@�\@�$�@��-@��`@�1@�|�@�R@�=q@���@��@�(�@��@�+@�ff@�J@�`B@�1'@�1@�  @��
@�@�;d@��y@⟾@��@�O�@�/@�&�@�V@�9@�|�@�o@޸R@�v�@�M�@���@ܛ�@��
@���@��@�"�@�@�~�@�-@��@�z�@���@ם�@�
=@���@պ^@�-@�{@�@�X@��@�1'@ӍP@�33@ҧ�@�M�@��@���@�X@�1@υ@�o@���@Ώ\@�$�@���@�p�@��@̣�@�1'@�ƨ@�t�@�"�@���@�ȴ@�v�@�5?@���@�`B@��@��/@�r�@�I�@�9X@��m@ǥ�@ǝ�@�33@���@�M�@���@�@ŉ7@�&�@��@�Ĝ@�z�@��;@��@�5?@�x�@���@�Z@���@��@���@�ff@�=q@�-@�{@��^@���@�x�@�O�@�/@��@�z�@��@��H@�x�@�%@���@���@�  @�\)@�o@��y@���@�=q@��@���@���@�X@�7L@�Ĝ@�+@�
=@�5?@�@�G�@��@��@��@�1'@��H@�v�@�=q@�{@��#@���@�`B@�V@��9@�  @�|�@�dZ@�"�@�ȴ@��\@�V@�$�@��@��-@�p�@�?}@��j@�Q�@��@�
=@�;d@�;d@��!@���@��!@�5?@���@���@��@�X@�&�@���@�Ĝ@�9X@�;d@���@��y@�^5@��#@��@�V@�A�@���@�|�@�t�@��@�l�@�dZ@��@�V@���@��@��@��@���@�X@��@���@�Q�@�1'@�1@��;@��P@�;d@�@��R@���@�ff@�5?@��@��#@���@�@���@�hs@���@��@���@��@���@�Z@��@��
@�ƨ@��F@��P@�|�@�o@�ȴ@�~�@�^5@�=q@��@���G�O�@�^5@���@��D@��
@~@s"�@fV@a��@Y��@R=q@J~�@A��@:�@3ƨ@,I�@(�@#��@�P@-@V@hs11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBǮBǮBǮBǮBǮBǮBǮBƨBƨBƨBƨBƨBƨBƨBƨBƨBƨBƨBƨBƨBƨBƨBƨBƨBƨBƨBƨBƨBƨBƨBƨBƨBƨBƨBƨBƨBƨBƨBƨBǮBǮBǮBǮBǮBǮBǮBǮBƨB��B%�B�oB�hB�uB��B��B��B��B��B��B��B�=B� Bu�BhsBbNB`BBZBR�BC�B �B&�B,B49B6FBE�BZBbNBgmB`BB[#B;dBbNB[#BR�BM�BM�BL�BM�BF�BB�B�B�B��BǮBB�XB�-B��B��B�+Bz�Br�BcTBD�B�B
��B
�B
�B
ǮB
�XB
�!B
��B
��B
�%B
s�B
^5B
C�B
0!B
 �B
%B	�BB	B	z�B	C�B		7B�B�yB�`B�;B�B�B��B��B��B��BɺBǮBȴB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�
B��BǮBÖB�wB�dB�B�B��B��B�'B�B	%B	bB	bB	bB	hB	�B	�B	�B	�B	 �B	 �B	!�B	!�B	"�B	"�B	#�B	#�B	&�B	)�B	-B	1'B	9XB	:^B	A�B	F�B	H�B	I�B	P�B	W
B	[#B	]/B	\)B	YB	XB	aHB	dZB	cTB	gmB	m�B	n�B	l�B	k�B	jB	iyB	hsB	hsB	iyB	dZB	aHB	`BB	^5B	]/B	\)B	]/B	^5B	^5B	_;B	_;B	`BB	`BB	bNB	dZB	gmB	gmB	hsB	jB	k�B	m�B	o�B	q�B	r�B	t�B	u�B	v�B	x�B	z�B	z�B	z�B	y�B	x�B	x�B	x�B	�B	�%B	�%B	�%B	�%B	�1B	�JB	�PB	�PB	�VB	�VB	�\B	�\B	�bB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�!B	�3B	�?B	�LB	�LB	�?B	�LB	�^B	�dB	�dB	�jB	�qB	�}B	��B	��B	ÖB	ĜB	ƨB	ǮB	ƨB	ÖB	ÖB	ĜB	ŢB	ŢB	ƨB	ƨB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�B	�#B	�/B	�/B	�5B	�;B	�BB	�BB	�BB	�HB	�HB	�HB	�NB	�TB	�`B	�`B	�fB	�fB	�fB	�fB	�fB	�fB	�fB	�fB	�fB	�fB	�fB	�mB	�mB	�mB	�mB	�mB	�mB	�mB	�mB	�mB	�fB	�fB	�fB	�fB	�fB	�mB	�mB	�mB	�mB	�mB	�fB	�ZB	�`B	�ZB	�ZB	�TB	�TB	�TB	�ZB	�`B	�TB	�TB	�TB	�TB	�TB	�TB	�ZB	�fB	�fB	�fB	�`B	�fB	�fB	�fB	�mB	�sB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
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
%B
%B
%B
1B
	7B
\B
�B
�B
 �B
'�B
.B
8RB
:^B
B�B
D�B
J�B
XB
\)B
bNB
e`B
iyB
m�B
p�B
r�B
u�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   BǃBǄBǄBǄBǆBǆBǄB�}B�{B�}B�}B�}B�zB�}B�B�}B�}B�{B�}B�}B�}B�{B�}B�}B�}B�}B�}B�{B�{B�}B�}B�}B�{B�}B�}B�}B�}B�}B�{BǆBǈBǆBǆBǆBǆBǈBǄB�{B˝B%�B�FB�@B�MB�bB�qB��B��B��B��B�\B�B�Bu�BhJBb$B`BY�BR�BCjB �B&�B+�B4B6BEwBY�Bb!BgDB`BZ�B;6Bb"BZ�BR�BM�BM�BL�BM�BFzBBaBYB�[B��B�~B�_B�)B��B��B�QB��Bz�Br�Bc BDiBQB
��B
�wB
��B
�~B
�+B
��B
��B
�dB
��B
s�B
^B
CjB
/�B
 �B
�B	�B	�eB	z�B	CoB		B�xB�RB�9B�B��B��B��B��BδB̨BɔBǇBȎBʙBʜB˟B˟BϹB��BϷBʜBͫBͬB̧B��B��B��B��B��B��B��BϵBǅB�nB�OB�:B��B��B��B��B��B��B	�B	5B	8B	6B	:B	lB	tB	sB	�B	 �B	 �B	!�B	!�B	"�B	"�B	#�B	#�B	&�B	)�B	,�B	0�B	9(B	:/B	A[B	FyB	H�B	I�B	P�B	V�B	Z�B	\�B	[�B	X�B	W�B	aB	d(B	c#B	g;B	m_B	neB	lYB	kSB	jMB	iGB	hCB	hBB	iDB	d)B	aB	`B	^B	\�B	[�B	\�B	^B	^B	_
B	_	B	`B	`B	bB	d&B	g;B	g;B	hAB	jLB	kQB	m]B	ojB	qwB	r}B	t�B	u�B	v�B	x�B	z�B	z�B	z�B	y�B	x�B	x�B	x�B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�"B	�B	�'B	�&B	�,B	�8B	�QB	�QB	�RB	�RB	�^B	�dB	�dB	�iB	�lB	�jB	�zB	�oB	�yB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�&B	�.B	�.B	�2B	�;B	�DB	�JB	�TB	�]B	�dB	�pB	�tB	�pB	�\B	�`B	�eB	�kB	�kB	�oB	�oB	�{B	ɂB	ʊB	ˎB	̔B	̕B	͛B	͜B	͜B	ΡB	ϥB	ѶB	һB	ҼB	ҺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�	B	�	B	�B	�B	�B	�B	�B	�B	�'B	�%B	�,B	�+B	�,B	�.B	�+B	�-B	�-B	�.B	�+B	�,B	�*B	�2B	�4B	�1B	�2B	�4B	�2B	�3B	�3B	�2B	�,B	�,B	�+B	�-B	�+B	�6B	�4B	�2B	�4B	�3B	�,B	�!B	�)B	� B	�!B	�B	�B	�B	�!B	�%B	�B	�B	�B	�B	�B	�B	� B	�,B	�,B	�+B	�'B	�,B	�+B	�)B	�3B	�9B	�?B	�?B	�@B	�>B	�EB	�SB	�JB	�CB	�>B	�kB	�tB	�jB	�iB	�vB	�pB	�pB	�pB	�jB	�qB	�pB	�lB	�iB	�aB	�iB	�oB	�uB	�tB	�iB	�iB	�gB	�dB	�dB	�cB	�iB	�sB	�sB	�uB	�rB	�iB	�mB	�tB	�uB	�zB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
�B
�B
�B
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
�B
�B
�G�O�B
�B
 B
MB
hB
 �B
'�B
-�B
8B
:B
BSB
D`B
J�B
W�B
[�B
bB
e#B
i<B
mTB
piB
rrB
u�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.51 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451132016080714511320160807145113  AO  ARCAADJP                                                                    20150226221429    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150226221429  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150226221429  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145113  IP                  G�O�G�O�G�O�                