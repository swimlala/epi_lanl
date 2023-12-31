CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-12-17T03:17:46Z AOML 3.0 creation; 2016-08-07T21:51:24Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20151217031746  20160807145124  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               ]A   AO  5287_9017_093                   2C  D   APEX                            6529                            072314                          846 @׆�g(�O1   @׆�FG�@0��l�C��d�=p��
1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    ]A   B   B   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C�fC  C  C  C  C  C  C  C   C"  C$  C&  C'�fC*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~�C�  C�  C�  C��C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DBfDB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  DtffDy� D���D�@ D��3D�ٚD� D�FfD��D��fD�fD�33D��fD���D�  D�I�D�|�D�fD�fD�<�D�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@�Q�A(�A((�AH(�Ah(�A�{A�{A�{A�{A�{A�{A�{A�{B
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
=B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�8RB���B�B�B�B�B�B�B�B�B�B�B�B�B�B�C ��C��C��C��C��C
��C��C��Ch�C��C��C��C��C��C��C��C ��C"��C$��C&��C(h�C*��C,��C.��C0��C2��C4��C6��C8��C:��C<��C>��C@��CB��CD��CF��CH��CJ��CL��CN��CP��CR��CT��CV��CX��CZ��C\��C^��C`��Cb��Cd��Cf��Ch��Cj��Cl��Cn��Cp��Cr��Ct��Cv��Cx��Cz��C|��C~�)C�AHC�AHC�AHC�NC�AHC�AHC�AHC�AHC�AHC�4{C�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHD  �D ��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D	 �D	��D
 �D
��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D>D��D �D��D �D�
D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D  �D ��D! �D!��D" �D"��D# �D#��D$ �D$��D% �D%��D& �D&��D' �D'��D( �D(��D) �D)��D* �D*��D+ �D+��D, �D,��D- �D-��D. �D.��D/ �D/��D0 �D0��D1 �D1��D2 �D2��D3 �D3��D4 �D4��D5 �D5��D6 �D6��D7 �D7��D8 �D8��D9 �D9��D: �D:��D; �D;��D< �D<��D= �D=��D> �D>��D? �D?��D@ �D@��DA �DA��DB'
DB��DC �DC��DD �DD��DE �DE��DF �DF��DG �DG��DH �DH��DI �DI��DJ �DJ��DK �DK��DL �DL��DM �DM��DN �DN��DO �DO��DP �DP��DQ �DQ��DR �DR��DS �DS��DT �DT��DU �DU��DV �DV��DW �DW��DX �DX��DY �DY��DZ �DZ��D[ �D[��D\ �D\��D] �D]��D^ �D^��D_ �D_��D` �D`��Da �Da��Db �Db��Dc �Dc��Dd �Dd��De �De��Df �Df��Dg �Dg��Dh �Dh��Di �Di��Dj �Dj��Dk �Dk��Dl �Dl��Dm �Dm��Dn �Dn��Do �Do��Dp �Dp��Dq �Dq��Dr �Dr��Ds �Ds��Dt �Dt�
Dy�D�D�PRD���D���D� RD�V�D�D�ָD��D�C�D���D��D�RD�Y�DڍD�ƸD��D�MD��D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�;dA�?}A�;dA�/A�7LA�33A�-A�33A�5?A�;dA�?}A�?}A�?}A�A�A�A�A�A�A�A�A�?}A�?}A�=qA�=qA�;dA�7LA�7LA�;dA�=qA�;dA�;dA�9XA�7LA�9XA�9XA�;dA�=qA�=qA�=qA�?}A�?}A�?}A�A�A�?}A�?}A�=qA�7LA�1'A�%A֟�A�7LA�hsA˛�A�&�AȍPA�x�A�^5A�l�A�-A��Aė�A�S�A�x�A���A���A��#A���A�ȴA���A�
=A���A��HA�M�A�JA�hsA���A��A�ZA�^5A�jA��wA�A��hA�(�A��A���A�JA�&�A�z�A��A��#A�5?A���A�t�A�VA��9A�~�A���A�~�A��
A���A���A��A�  A�JA��A�?}A�wA~1'A|�HA{�^Ay�hAvjAqO�Amx�AhĜAf��Af �Ad�HAc�
Ac��Aa�TA`-A^��A\$�AW;dAT$�ARr�AQl�AO�TANr�AL��AJ�/AHQ�AB��A?A>�HA=�wA<VA9�A7�7A6��A5�TA57LA4�9A4 �A2��A17LA0ffA01'A/��A/�A-�A+��A*�RA*(�A)G�A%\)A#��A"�A!VA!�A ��A �A�A��A��AO�A
=A��AAdZA�#Ax�Ap�AO�A�AhsA�`A�+A1A�Ap�AhsAhsA\)AVA�mA�wAl�A�RA�A��AC�A�A �A�-A�FAƨA�A{A{A�mA
��A	��Ar�Ap�AoAn�A{AbA\)A�\A�A��A��A33AjA(�A�wAx�A �9A J@���@��-@� �@��@��@��;@�ȴ@�b@���@�&�@���@��@��y@��@���@�w@�dZ@��y@@�h@�I�@�P@땁@�+@���@��`@�Ĝ@�b@��@�7@��/@�z�@�^5@��@�j@߶F@�@�J@�&�@ܣ�@�ƨ@�C�@۾w@؋D@�K�@�5?@ա�@�r�@�j@�?}@�"�@��H@�=q@�x�@ёh@���@�(�@�/@�x�@љ�@�O�@мj@ЋD@�dZ@ͺ^@�1@�|�@�l�@�ȴ@ɺ^@�`B@ȣ�@���@Ɂ@ɩ�@ȓu@Ǯ@�dZ@�;d@��y@�n�@�{@�X@ģ�@�1'@�l�@�+@��@�@���@�?}@�&�@���@�z�@���@���@��P@��@�{@��@���@�hs@�x�@�O�@���@�r�@�9X@�1@�b@�b@��;@��@��\@�n�@�E�@�$�@��#@���@���@���@�x�@�p�@��@�1@�1'@�9X@�(�@��
@��P@�o@�M�@�{@��T@�@�X@��`@���@�Z@���@���@�$�@���@�?}@��/@��9@�Q�@��@���@�;d@��@��H@���@�M�@��T@��^@���@��h@�7L@���@�bN@�  @��P@���@��+@��@���@��@�  @���@���@�ƨ@��w@��F@���@�C�@�ff@�=q@�J@��T@���@���@�hs@�/@���@��@�Q�@��;@��P@�C�@�"�@�@�ȴ@���@��+@�ff@�-@�p�@���@���@��@�bN@�9X@��F@��@���@��\@�n�@�ff@�^5@�5?@���@���@�`B@�%@���@��D@�9X@��
@���@��w@�l�@�@��@���@��R@�-@��#@��^@��h@�x�@�O�@��/@�Q�@�9X@���@�ƨ@�t�@��@���@�^5@�J@��^@���@���@��@�O�@�7L@�V@���@���@��@�bN@�b@��;@��w@���@�S�@��@��@��!@�-@���@��-@���@���@���@�`B@�O�@�@�l�@���@t�/@f�y@]?}@U?}@N��@G
=@?K�@8��@3��@.��@)��@&��@;d@��@��@^5@�@	hs11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A�;dA�?}A�;dA�/A�7LA�33A�-A�33A�5?A�;dA�?}A�?}A�?}A�A�A�A�A�A�A�A�A�?}A�?}A�=qA�=qA�;dA�7LA�7LA�;dA�=qA�;dA�;dA�9XA�7LA�9XA�9XA�;dA�=qA�=qA�=qA�?}A�?}A�?}A�A�A�?}A�?}A�=qA�7LA�1'A�%A֟�A�7LA�hsA˛�A�&�AȍPA�x�A�^5A�l�A�-A��Aė�A�S�A�x�A���A���A��#A���A�ȴA���A�
=A���A��HA�M�A�JA�hsA���A��A�ZA�^5A�jA��wA�A��hA�(�A��A���A�JA�&�A�z�A��A��#A�5?A���A�t�A�VA��9A�~�A���A�~�A��
A���A���A��A�  A�JA��A�?}A�wA~1'A|�HA{�^Ay�hAvjAqO�Amx�AhĜAf��Af �Ad�HAc�
Ac��Aa�TA`-A^��A\$�AW;dAT$�ARr�AQl�AO�TANr�AL��AJ�/AHQ�AB��A?A>�HA=�wA<VA9�A7�7A6��A5�TA57LA4�9A4 �A2��A17LA0ffA01'A/��A/�A-�A+��A*�RA*(�A)G�A%\)A#��A"�A!VA!�A ��A �A�A��A��AO�A
=A��AAdZA�#Ax�Ap�AO�A�AhsA�`A�+A1A�Ap�AhsAhsA\)AVA�mA�wAl�A�RA�A��AC�A�A �A�-A�FAƨA�A{A{A�mA
��A	��Ar�Ap�AoAn�A{AbA\)A�\A�A��A��A33AjA(�A�wAx�A �9A J@���@��-@� �@��@��@��;@�ȴ@�b@���@�&�@���@��@��y@��@���@�w@�dZ@��y@@�h@�I�@�P@땁@�+@���@��`@�Ĝ@�b@��@�7@��/@�z�@�^5@��@�j@߶F@�@�J@�&�@ܣ�@�ƨ@�C�@۾w@؋D@�K�@�5?@ա�@�r�@�j@�?}@�"�@��H@�=q@�x�@ёh@���@�(�@�/@�x�@љ�@�O�@мj@ЋD@�dZ@ͺ^@�1@�|�@�l�@�ȴ@ɺ^@�`B@ȣ�@���@Ɂ@ɩ�@ȓu@Ǯ@�dZ@�;d@��y@�n�@�{@�X@ģ�@�1'@�l�@�+@��@�@���@�?}@�&�@���@�z�@���@���@��P@��@�{@��@���@�hs@�x�@�O�@���@�r�@�9X@�1@�b@�b@��;@��@��\@�n�@�E�@�$�@��#@���@���@���@�x�@�p�@��@�1@�1'@�9X@�(�@��
@��P@�o@�M�@�{@��T@�@�X@��`@���@�Z@���@���@�$�@���@�?}@��/@��9@�Q�@��@���@�;d@��@��H@���@�M�@��T@��^@���@��h@�7L@���@�bN@�  @��P@���@��+@��@���@��@�  @���@���@�ƨ@��w@��F@���@�C�@�ff@�=q@�J@��T@���@���@�hs@�/@���@��@�Q�@��;@��P@�C�@�"�@�@�ȴ@���@��+@�ff@�-@�p�@���@���@��@�bN@�9X@��F@��@���@��\@�n�@�ff@�^5@�5?@���@���@�`B@�%@���@��D@�9X@��
@���@��w@�l�@�@��@���@��R@�-@��#@��^@��h@�x�@�O�@��/@�Q�@�9X@���@�ƨ@�t�@��@���@�^5@�J@��^@���@���@��@�O�@�7L@�V@���@���@��@�bN@�b@��;@��w@���@�S�@��@��@��!@�-@���@��-@���@���@���@�`BG�O�@�@�l�@���@t�/@f�y@]?}@U?}@N��@G
=@?K�@8��@3��@.��@)��@&��@;d@��@��@^5@�@	hs11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB  BB  B  BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB%B+B9XB0!B\)B�PB�oB�hB�bB��B��B��B�\B�1B�Bp�BdZB��B��B�B�'B�!B�!B�!B�9B�LBȴBɺB��B�hBr�BgmB�1B��Bp�B,B�/B��B�B��B�Bn�Bk�BbNBD�B\B	7B;dB�B�BhB,B�B  B
�mB
�5B
ŢB
��B
�+B
p�B
aHB
T�B
K�B
:^B
�B	��B	�B	�dB	�B	��B	��B	��B	�hB	�7B	}�B	p�B	`BB	F�B	7LB	.B	(�B	!�B	�B	�B	PB	  B�mB�/B�B��B��B��B�
B�`B��B��B��B��B	1B	�B	"�B	#�B	&�B	+B	>wB	G�B	I�B	G�B	D�B	8RB	33B	0!B	0!B	6FB	9XB	@�B	?}B	<jB	:^B	=qB	E�B	H�B	G�B	C�B	@�B	H�B	K�B	P�B	W
B	ZB	XB	XB	W
B	VB	W
B	XB	YB	ZB	YB	W
B	P�B	N�B	I�B	G�B	G�B	H�B	J�B	L�B	Q�B	VB	W
B	[#B	^5B	_;B	bNB	aHB	`BB	`BB	bNB	bNB	cTB	cTB	bNB	dZB	ffB	ffB	hsB	iyB	iyB	iyB	k�B	k�B	k�B	k�B	jB	n�B	q�B	r�B	r�B	s�B	w�B	t�B	o�B	l�B	k�B	p�B	w�B	}�B	� B	�%B	�7B	�DB	�PB	�VB	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	�'B	�9B	�FB	�^B	�qB	�qB	�jB	�^B	�XB	�jB	�wB	�wB	ÖB	ĜB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	ȴB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�B	�#B	�)B	�/B	�5B	�;B	�;B	�;B	�;B	�;B	�;B	�;B	�5B	�BB	�HB	�NB	�NB	�NB	�NB	�TB	�`B	�`B	�fB	�mB	�mB	�mB	�fB	�fB	�fB	�fB	�`B	�fB	�mB	�fB	�`B	�`B	�`B	�`B	�`B	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
  B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
%B
1B
1B
1B
1B
1B
1B
1B
1B
1B
1B
1B
1B
1B
1B
	7B

=B
DB
JB
PB
VB
VB
\B
\B
bB
bB
bB
bB
bB
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
uB
uB
uB
uB
uB
uB
uB
{B
{B
�B
�B
�B
�B
&�B
2-B
9XB
>wB
C�B
I�B
N�B
VB
ZB
^5B
bNB
e`B
l�B
p�B
u�B
y�B
|�B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B
��B �B
��B
��B �B �B �B �B �B �B �B �B �B �B�B �B �B �B �B �B �B �B �B �B �B �B �B �B �B �B�B �B �B �B �B�B �B�B�B �B�B�B�B�B�B�B*�B9.B/�B\B�&B�EB�@B�8B�YB�bB�XB�4B�B��BpxBd,B�vB��B��B��B��B��B��B�B�BȈBɊB��B�;Br�Bg?B�B��BpqB+�B��BϮB��B�VB��BnjBkRBbBDmB)B	B;3B�B�B8B+�BQB
��B
�<B
�B
�qB
��B
��B
pwB
aB
T�B
K�B
:0B
�B	��B	��B	�6B	��B	��B	��B	�YB	�>B	�B	}�B	p|B	`B	F�B	7#B	-�B	(�B	!�B	}B	ZB	(B��B�GB�B��B��BϺB˟B��B�7B��B��B��B��B	
B	�B	"�B	#�B	&�B	*�B	>HB	G�B	I�B	G�B	DoB	8%B	3B	/�B	/�B	6B	9(B	@VB	?OB	<<B	:/B	=BB	EtB	H�B	GB	CiB	@SB	H�B	K�B	P�B	V�B	Y�B	W�B	W�B	V�B	U�B	V�B	W�B	X�B	Y�B	X�B	V�B	P�B	N�B	I�B	G~B	G�B	H�B	J�B	L�B	Q�B	U�B	V�B	Z�B	^B	_B	bB	aB	`B	`B	bB	bB	c#B	c#B	bB	d(B	f3B	f2B	hCB	iEB	iFB	iFB	kSB	kSB	kRB	kSB	jKB	nfB	qwB	r}B	r|B	s�B	w�B	t�B	olB	lVB	kPB	poB	w�B	}�B	�B	��B	�B	�B	�B	�B	�/B	�CB	�QB	�lB	�vB	�]B	�QB	�cB	�jB	�wB	��B	��B	��B	�wB	�bB	�iB	�jB	�gB	�qB	��B	��B	��B	��B	��B	��B	�zB	�wB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�&B	�9B	�:B	�4B	�%B	�!B	�.B	�>B	�AB	�]B	�fB	�qB	ɀB	ϤB	ҹB	ϦB	͚B	̔B	ˌB	ˏB	̕B	ˍB	�|B	�zB	�|B	ʊB	̓B	̖B	͘B	ΞB	ΟB	ϥB	ϥB	ϢB	ЫB	ѳB	ѳB	ЬB	ЫB	ЫB	ҹB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�B	�B	�B	� B	� B	��B	�B	�B	�B	�B	�B	�B	�B	�&B	�(B	�*B	�4B	�3B	�3B	�+B	�,B	�+B	�+B	�&B	�*B	�3B	�+B	�(B	�&B	�&B	�'B	�(B	�3B	�CB	�UB	�LB	�CB	�DB	�DB	�LB	�QB	�`B	�iB	�gB	�iB	�cB	�cB	�`B	�\B	�_B	�cB	�bB	�sB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B

B
B
B
B
B
B
!B
!B
'B
&B
$B
$B
'B
'B
(B
&B
)B
+B
)B
,B
+B
,B
,B
+B
4B
3B
8B
7B
9B
8B
9B
;B
9B
?B
=G�O�B
JB
\B
�B
&�B
1�B
9B
>8B
CZB
IyB
N�B
U�B
Y�B
]�B
bB
e!B
lJB
pfB
u�B
y�B
|�B
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.51 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451242016080714512420160807145124  AO  ARCAADJP                                                                    20151217031746    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20151217031746  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20151217031746  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145124  IP                  G�O�G�O�G�O�                