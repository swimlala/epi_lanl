CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2016-05-23T09:15:34Z AOML 3.0 creation; 2016-08-07T21:51:29Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20160523091534  20160825183417  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               {A   AO  5287_9017_123                   2C  D   APEX                            6529                            072314                          846 @׮IaG��1   @׮I�O��@0W
=p���d؋C��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    {A   B   B   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
�C  C  C  C  C  C  C  C  C  C�C �C"  C#�fC&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� DtfDts3Dy��D��D�S3D�vfD�ɚD��D�0 D�S3D�ɚD�3D�Y�D���D�ɚD� D�FfDډ�D��3D�3D�L�D�D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��R@�Q�A(�A((�AH(�Ah(�A�{A�{A�{A�{A�{A�{A�{A�{B
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
=B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C ��C��C��C��C��C
�)C��C��C��C��C��C��C��C��C��C�)C �)C"��C$h�C&��C(��C*��C,��C.��C0��C2��C4��C6��C8��C:��C<��C>��C@��CB��CD��CF��CH��CJ��CL��CN��CP��CR��CT��CV��CX��CZ��C\��C^��C`��Cb��Cd��Cf��Ch��Cj��Cl��Cn��Cp��Cr��Ct��Cv��Cx��Cz��C|��C~��C�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�NC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHD  �D ��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D	 �D	��D
 �D
��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D  �D ��D! �D!��D" �D"��D# �D#��D$ �D$��D% �D%��D& �D&��D' �D'��D( �D(��D) �D)��D* �D*��D+ �D+��D, �D,��D- �D-��D. �D.��D/ �D/��D0 �D0��D1 �D1��D2 �D2��D3 �D3��D4 �D4��D5 �D5��D6 �D6��D7 �D7��D8 �D8��D9 �D9��D: �D:��D; �D;��D< �D<��D= �D=��D> �D>��D? �D?��D@ �D@��DA �DA��DB �DB��DC �DC��DD �DD��DE �DE��DF �DF��DG �DG��DH �DH��DI �DI��DJ �DJ��DK �DK��DL �DL��DM �DM��DN �DN��DO �DO��DP �DP��DQ �DQ��DR �DR��DS �DS��DT �DT��DU �DU��DV �DV��DW �DW��DX �DX��DY �DY��DZ �DZ��D[ �D[��D\ �D\��D] �D]��D^ �D^��D_ �D_��D` �D`��Da �Da��Db �Db��Dc �Dc��Dd �Dd��De �De��Df �Df��Dg �Dg��Dh �Dh��Di �Di��Dj �Dj��Dk �Dk��Dl �Dl��Dm �Dm��Dn �Dn��Do �Do��Dp �Dp��Dq �Dq��Dr �Dr��Ds �Ds��Dt'
Dt��Dy�qD�)�D�c�D���D���D�D�@RD�c�D���D�#�D�i�D��D���D� RD�V�Dڙ�D��D��D�]D��D��R11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��#A���A���A���Aհ!A�p�A�K�A��
Aԛ�A�z�A�l�A�dZA�^5A�\)A�XA�S�A�O�A�I�A�G�A�E�A�E�A�G�A�G�A�G�A�G�A�E�A�C�A�;dA�&�A�  A�7LA��A�AғuA�7LA�%A��HAѕ�A�VA�M�A���A�E�A΃A�jA�x�A�33A�A˛�Aʰ!A�dZA�bNA��TA�ĜA�v�A�  A�A�A��
A�K�A�A�ZA�z�A�$�A��A���A�bNA�oA���A���A���A��A���A��yA�JA�&�A��PA�hsA��RA��A�E�A��A�A�A��HA��A��-A���A��A���A�|�A�t�A�\)A���A���A�;dA���A��+A��!A��A�1'A�G�A��!A��A~{A{�hAx�/AwhsAvZAu&�AtA�Aq�
AoVAl~�Ak�PAk"�Aj=qAi"�AhA�Af��Ae��AdĜAb��Aa;dA_�-A\��AX��AS�AN��AMXAH��AD��A@�jA>9XA;�FA9�FA9"�A7��A7A6I�A5;dA4�DA4(�A2�HA0=qA/�FA.�/A,�DA+��A+oA)�FA(�DA&ȴA%|�A$ffA#�PA!�;A �A/A��A�jA�9A��A��AC�A�FAp�Al�AJA�DA�DA{AA��A�+A�AG�AA�A �A�wA�AS�A%A��A�FAA	�A�
AbA�A��A�FA��A7LAz�A33A7LA�A
=A
=A
=A�AVA�A��A��A^5A Z@���A A Q�@��
@��-@�K�@��R@�@�`B@���@�K�@�Ĝ@�@�\@�n�@�M�@�@��@��T@�Ĝ@���@�?}@��#@�M�@�\@�J@��@��m@�$�@�I�@���@�9X@�O�@ۥ�@�v�@�`B@�%@��/@أ�@�1@ם�@���@և+@և+@�{@�p�@�z�@Ӯ@�o@�ff@Ѳ-@��@Ь@�r�@�bN@�j@�bN@�I�@�1@Ͼw@υ@��H@�v�@�E�@��T@��/@�I�@�1@˅@�"�@�$�@�/@��@��@ȼj@ȣ�@ȓu@�z�@�|�@�=q@�p�@�V@��/@ļj@�A�@��@�S�@��@�@�J@��@�x�@���@�(�@�1@���@���@��y@�5?@�{@�J@���@��@���@���@��@�O�@���@��9@��@�I�@��@�ƨ@���@�;d@���@�{@���@��@�@���@�p�@�hs@�`B@�O�@�&�@�j@�ƨ@���@�
=@���@�~�@�$�@���@�hs@�&�@��@�%@��`@��D@�Z@�1@��@���@�dZ@�"�@��@�n�@�-@��@�@�hs@��u@�1@���@���@��@�t�@�\)@�+@��@���@�$�@�@��@��@��#@��7@�G�@�/@�V@��@��j@�9X@��m@���@�33@���@���@�v�@�{@�V@�Z@��
@��w@���@�dZ@�33@��@���@�ff@�@�X@���@�1@���@�\)@�
=@���@��T@��@�x�@�hs@�?}@�&�@��`@���@�z�@�bN@�b@�ƨ@��w@��w@���@�l�@�C�@�+@�
=@���@�n�@���@�@��@���@���@�z�@�Z@��m@��F@��@�dZ@��@��@�ȴ@�~�@�n�@�-@���@���@�hs@��@���@�r�@�1@��@��m@���@�dZ@�S�@�C�@�33@�o@��\@�E�@��@��h@�?}@��@���@���@�I�@�(�@���@�t�@�K�@�o@��!@�n�@�^5@�$�@��#@���@�p�@�7L@��@�%@��@���@�r�@�Z@�  @��w@���@�O�@�9X@��y@{C�@t(�@k�
@a�7@\I�@U��@Ix�@?�@81'@2�H@-�T@'�@!�^@�@�u@x�@@	��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A��#A���A���A���Aհ!A�p�A�K�A��
Aԛ�A�z�A�l�A�dZA�^5A�\)A�XA�S�A�O�A�I�A�G�A�E�A�E�A�G�A�G�A�G�A�G�A�E�A�C�A�;dA�&�A�  A�7LA��A�AғuA�7LA�%A��HAѕ�A�VA�M�A���A�E�A΃A�jA�x�A�33A�A˛�Aʰ!A�dZA�bNA��TA�ĜA�v�A�  A�A�A��
A�K�A�A�ZA�z�A�$�A��A���A�bNA�oA���A���A���A��A���A��yA�JA�&�A��PA�hsA��RA��A�E�A��A�A�A��HA��A��-A���A��A���A�|�A�t�A�\)A���A���A�;dA���A��+A��!A��A�1'A�G�A��!A��A~{A{�hAx�/AwhsAvZAu&�AtA�Aq�
AoVAl~�Ak�PAk"�Aj=qAi"�AhA�Af��Ae��AdĜAb��Aa;dA_�-A\��AX��AS�AN��AMXAH��AD��A@�jA>9XA;�FA9�FA9"�A7��A7A6I�A5;dA4�DA4(�A2�HA0=qA/�FA.�/A,�DA+��A+oA)�FA(�DA&ȴA%|�A$ffA#�PA!�;A �A/A��A�jA�9A��A��AC�A�FAp�Al�AJA�DA�DA{AA��A�+A�AG�AA�A �A�wA�AS�A%A��A�FAA	�A�
AbA�A��A�FA��A7LAz�A33A7LA�A
=A
=A
=A�AVA�A��A��A^5A Z@���A A Q�@��
@��-@�K�@��R@�@�`B@���@�K�@�Ĝ@�@�\@�n�@�M�@�@��@��T@�Ĝ@���@�?}@��#@�M�@�\@�J@��@��m@�$�@�I�@���@�9X@�O�@ۥ�@�v�@�`B@�%@��/@أ�@�1@ם�@���@և+@և+@�{@�p�@�z�@Ӯ@�o@�ff@Ѳ-@��@Ь@�r�@�bN@�j@�bN@�I�@�1@Ͼw@υ@��H@�v�@�E�@��T@��/@�I�@�1@˅@�"�@�$�@�/@��@��@ȼj@ȣ�@ȓu@�z�@�|�@�=q@�p�@�V@��/@ļj@�A�@��@�S�@��@�@�J@��@�x�@���@�(�@�1@���@���@��y@�5?@�{@�J@���@��@���@���@��@�O�@���@��9@��@�I�@��@�ƨ@���@�;d@���@�{@���@��@�@���@�p�@�hs@�`B@�O�@�&�@�j@�ƨ@���@�
=@���@�~�@�$�@���@�hs@�&�@��@�%@��`@��D@�Z@�1@��@���@�dZ@�"�@��@�n�@�-@��@�@�hs@��u@�1@���@���@��@�t�@�\)@�+@��@���@�$�@�@��@��@��#@��7@�G�@�/@�V@��@��j@�9X@��m@���@�33@���@���@�v�@�{@�V@�Z@��
@��w@���@�dZ@�33@��@���@�ff@�@�X@���@�1@���@�\)@�
=@���@��T@��@�x�@�hs@�?}@�&�@��`@���@�z�@�bN@�b@�ƨ@��w@��w@���@�l�@�C�@�+@�
=@���@�n�@���@�@��@���@���@�z�@�Z@��m@��F@��@�dZ@��@��@�ȴ@�~�@�n�@�-@���@���@�hs@��@���@�r�@�1@��@��m@���@�dZ@�S�@�C�@�33@�o@��\@�E�@��@��h@�?}@��@���@���@�I�@�(�@���@�t�@�K�@�o@��!@�n�@�^5@�$�@��#@���@�p�@�7L@��@�%@��@���@�r�@�Z@�  @��wG�O�@�O�@�9X@��y@{C�@t(�@k�
@a�7@\I�@U��@Ix�@?�@81'@2�H@-�T@'�@!�^@�@�u@x�@@	��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
ȴB
ȴB
ȴB
ȴB
ǮB
ƨB
ŢB
ƨB
ƨB
ƨB
ƨB
ǮB
ƨB
ƨB
ǮB
ǮB
ǮB
ǮB
ǮB
ǮB
ǮB
ȴB
ȴB
ȴB
ȴB
ȴB
ȴB
��B
��B
�TB
��BJB�B�B%�B1'B5?B<jB?}B[#BjBv�B� B}�B��B��B�B�RB��BbB�B?}BS�BdZBffBr�Bv�B|�B�bB�PB�JB�\B��B��B��B��B��B�{BgmB^5B\)BR�BN�BN�BK�BM�BG�B@�B<jB:^B<jB2-BbBB��B��BȴB� B-BB
��B
�B
�B
��B
�B
��B
�!B
�'B
��B
�%B
hsB
Q�B
C�B
0!B
#�B
�B
bB
1B	��B	�yB	�5B	�#B	�5B	�BB	�BB	�5B	��B	��B	ƨB	�XB	�B	��B	�hB	w�B	R�B	:^B	0!B	�B		7B��B�B�B�sB�fB�TB�;B�)B�B�B��B��B��B��BȴBȴBǮBŢB��B�qB�^B�LB�XB�XB�XB�qB�wB�}B�XB�!B�B�!B�XB��B�BȴB��B�B�#B�TB��B��B��B	  B��B	DB	�B	�B	�B	�B	�B	\B	B	B	  B		7B	�B	!�B	%�B	(�B	+B	,B	1'B	5?B	=qB	@�B	@�B	@�B	?}B	>wB	8RB	7LB	;dB	R�B	Q�B	G�B	E�B	S�B	^5B	_;B	\)B	dZB	e`B	gmB	k�B	l�B	jB	dZB	`BB	bNB	hsB	jB	n�B	gmB	bNB	ffB	q�B	|�B	�1B	�VB	��B	��B	��B	��B	�uB	�7B	�B	v�B	q�B	p�B	r�B	s�B	t�B	s�B	t�B	u�B	y�B	{�B	|�B	|�B	}�B	�B	�B	�%B	�+B	�+B	�7B	�DB	�VB	�bB	�bB	�hB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�'B	�9B	�?B	�?B	�?B	�RB	�jB	�jB	�jB	�qB	�qB	�qB	�wB	�wB	�}B	��B	��B	��B	B	ÖB	ÖB	ĜB	ĜB	ƨB	ǮB	ǮB	ǮB	ȴB	ȴB	ȴB	ȴB	ȴB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�B	�#B	�)B	�/B	�/B	�5B	�5B	�5B	�5B	�;B	�;B	�BB	�BB	�BB	�BB	�BB	�NB	�NB	�TB	�TB	�TB	�TB	�`B	�fB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B	��B
  B
B
B
B
B
B
B
B
B
B
%B
%B
+B
1B
1B
1B
1B
1B
1B
1B
	7B

=B

=B

=B

=B

=B

=B
DB
DB
DB
DB
JB
PB
PB
PB
PB
PB
PB
PB
PB
PB
VB
VB
VB
\B
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
\B
\B
\B
bB
hB
hB
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
oB
�B
!�B
+B
0!B
49B
8RB
<jB
@�B
C�B
K�B
R�B
YB
`BB
dZB
hsB
m�B
q�B
u�B
y�B
~�B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B
ȋB
ȏB
ȒB
ȏB
ǋB
ƃB
�{B
ƂB
ƁB
ƁB
ƂB
ǇB
ƂB
ƂB
ǊB
ǊB
ǈB
ǈB
ǈB
ǊB
ǈB
ȏB
ȏB
ȏB
ȑB
ȑB
ȑB
ʞB
��B
�.B
��B(BiB�B%�B0�B5B<AB?YBZ�BjXBv�B�B}�B�eB��B��B�&B��B9B�B?SBS�Bd1Bf=Br�Bv�B|�B�7B�&B�B�1B�iB��B��B��B��B�MBgAB^B\ BR�BN�BN�BK�BM�BG�B@SB<:B:/B<;B2B2B�B��B��BȄB�B,�B�B
��B
�vB
�fB
зB
��B
��B
��B
��B
��B
��B
hEB
Q�B
CgB
/�B
#�B
sB
8B
B	��B	�LB	�
B	��B	�	B	�B	�B	�	B	��B	ͨB	�{B	�+B	��B	��B	�>B	w�B	R�B	:6B	/�B	xB		B��B�vB�]B�LB�?B�.B�B�B��B��B��BαB̥B˟BȍBȎBǆB�yB�bB�IB�7B�&B�/B�/B�.B�HB�PB�SB�/B��B��B��B�.B��B��BȌBͩB��B��B�(B��B��B��B��B��B	B	SB	eB	eB	eB	^B	0B	�B	 �B��B		B	bB	!�B	%�B	(�B	*�B	+�B	0�B	5B	=?B	@RB	@TB	@RB	?LB	>FB	8B	7B	;4B	R�B	Q�B	G|B	EqB	S�B	^B	_B	[�B	d)B	e,B	g8B	kTB	lZB	jJB	d(B	`B	bB	hAB	jJB	ndB	g9B	bB	f2B	qtB	|�B	��B	�B	�IB	��B	��B	�eB	�?B	�B	��B	v�B	qvB	pnB	r|B	s�B	t�B	s�B	t�B	u�B	y�B	{�B	|�B	|�B	}�B	��B	��B	��B	��B	��B	��B	�B	�B	�.B	�.B	�3B	�7B	�AB	�EB	�KB	�LB	�OB	�UB	�VB	�XB	�cB	�gB	�hB	�nB	�vB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�2B	�1B	�1B	�:B	�7B	�7B	�?B	�@B	�EB	�IB	�PB	�QB	�WB	�]B	�]B	�dB	�dB	�pB	�wB	�uB	�uB	�zB	�{B	�{B	�yB	�zB	�zB	�yB	ʇB	ˏB	ˎB	̒B	̓B	͚B	͚B	ϧB	ϤB	ЬB	ЬB	ЫB	ѱB	ҺB	ҹB	ӿB	ӿB	ӽB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�	B	�	B	�	B	�B	�B	�B	�B	�B	�B	�$B	�,B	�3B	�;B	�WB	�^B	�_B	�WB	�VB	�^B	�jB	�iB	�kB	�kB	�pB	�oB	�vB	�mB	�qB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B

B

B

B

B
	�B

B
B
B

B

B
B
B
B
B
B
B
B
B
B
B
B
B
B
!B
B
 B
B
&B
&B
'B
%B
%B
&B
(B
B
B
!B
&B
*B
*B
'B
&B
$B
%B
(B
&B
)B
&B
&B
,G�O�B
WB
!�B
*�B
/�B
3�B
8B
<)B
@DB
CXB
K�B
R�B
X�B
`B
dB
h5B
mSB
qlB
u�B
y�B
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.51 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451292016080714512920160807145129  AO  ARCAADJP                                                                    20160523091534    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160523091534  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160523091534  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145129  IP                  G�O�G�O�G�O�                