CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2016-06-24T02:17:30Z AOML 3.0 creation; 2016-08-07T21:51:30Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20160624021730  20160825183417  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  5287_9017_129                   2C  D   APEX                            6529                            072314                          846 @׶0���1   @׶1'ҕ�@0�E�����dԣ�
=q1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�33A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B���B�  B�  B�  B�  B�  B�33B�  B�  C   C  C  C  C  C
  C  C  C  C�C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dtl�Dy` D�  D�P D�|�D�ɚD�3D�P D��fD�ɚD�3D�6fD���D�� D�  D�FfD�|�D�� D� D�@ D�l�D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@�Q�A(�A((�AH(�Ah(�A�{A�{A�{A�{A�{A�{A�G�A�{B
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
=B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�8RB�B�B���B�B�B�B�B�B�8RB�B�C ��C��C��C��C��C
��C��C��C��C�)C��C��C��C��C��C��C ��C"��C$��C&��C(��C*��C,��C.��C0��C2��C4��C6��C8��C:��C<��C>��C@��CB��CD��CF��CH��CJ��CL��CN��CP��CR��CT��CV��CX��CZ��C\��C^��C`��Cb��Cd��Cf��Ch��Cj��Cl��Cn��Cp��Cr��Ct��Cv��Cx��Cz��C|��C~��C�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHD  �D ��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D	 �D	��D
 �D
��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D'
D��D  �D ��D! �D!��D" �D"��D# �D#��D$ �D$��D% �D%��D& �D&��D' �D'��D( �D(��D) �D)��D* �D*��D+ �D+��D, �D,��D- �D-��D. �D.��D/ �D/��D0 �D0��D1 �D1��D2 �D2��D3 �D3��D4 �D4��D5 �D5��D6 �D6��D7 �D7��D8 �D8��D9 �D9��D: �D:��D; �D;��D< �D<��D= �D=��D> �D>��D? �D?��D@ �D@��DA �DA��DB �DB��DC �DC��DD �DD��DE �DE��DF �DF��DG �DG��DH �DH��DI �DI��DJ �DJ��DK �DK��DL �DL��DM �DM��DN �DN��DO �DO��DP �DP��DQ �DQ��DR �DR��DS �DS��DT �DT��DU �DU��DV �DV��DW �DW��DX �DX��DY �DY��DZ �DZ��D[ �D[��D\ �D\��D] �D]��D^ �D^��D_ �D_��D` �D`��Da �Da��Db �Db��Dc �Dc��Dd �Dd��De �De��Df �Df��Dg �Dg��Dh �Dh��Di �Di��Dj �Dj��Dk �Dk��Dl �Dl��Dm �Dm��Dn �Dn��Do �Do��Dp �Dp��Dq �Dq��Dr �Dr��Ds �Ds��Dt �Dt�qDy��D�0RD�`RD��D���D�#�D�`RD���D���D��D�F�D���D��RD�RD�V�DڍD��RD� RD�PRD�}D��R11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�?}A�=qA�A�A�K�A�M�A�O�A�O�A�Q�A�Q�A�Q�A�S�A�S�A�S�A�S�A�Q�A�S�A�VA�XA�XA�ZA�S�A�XA�XA�XA�XA�ZA�\)A�\)A�VA�I�A�E�A�/A���AָRA�{A��TAծA�x�A�+A��`AӑhA�`BA���A��A�O�A���A��Aϰ!A��A�S�A�ȴA͙�A�n�A�A�A�bA�1'A�ffAɑhAƙ�A���A�bNAÕ�A�{A���A�p�A�G�A�M�A���A�^5A��mA�VA�^5A�ĜA��A��\A��DA�K�A�A�A��A�7LA��A�v�A�jA��RA�hsA�x�A���A�I�A�/A�t�A��+A��A�33A��A�$�A�G�AhsA}O�AudZAqC�Am\)Ai;dAe��AdVAcx�Ac�AadZA_�A^��A]�;A\��A[��AY�AW��AU�AT�ASG�ARȴAQK�ALVAJQ�AI��AH�/AHZAH{AG�mAFȴAD�+AC�
AB�AA��A@E�A?p�A>��A>z�A=�TA=�A;�PA7O�A69XA4�9A3G�A2r�A1�A0$�A/�7A.ȴA.^5A-A+&�A(n�A'�;A&�A%�
A$�!A#�A"-A ��At�A��AbNA$�A�A��A&�A �AdZA�!A�^A�A�A~�A�wA
=A=qAĜA$�A�A��A��A{A1'A{A��AXA�/A5?AA�A7LA��A�PA�A�`AjAp�A
=qA	?}A5?A�PAK�AĜAƨA��AZAl�A33AVA�A�HAĜA�!A��A�DA=qA�
AG�A �yA �HA ��A j@��
@�\)@���@�  @��@�{@�9X@��F@�C�@��y@�^5@��T@��`@��@�;d@�-@�p�@���@��@��D@�\)@�-@�@�@�A�@땁@���@�P@�C�@�n�@�E�@��@�hs@�`B@�7L@�r�@�F@�n�@�X@��/@��;@��y@���@�r�@�V@�V@�R@�n�@�/@�A�@�A�@�F@�K�@��H@⟾@��T@ᙚ@߶F@ޟ�@�E�@�V@��@�/@���@�
=@�O�@ץ�@��`@ӝ�@�C�@�M�@�5?@�{@�E�@�=q@�-@��@Ѳ-@��`@�j@Гu@мj@���@�Ĝ@�z�@�I�@��m@��y@Ώ\@Ͳ-@�%@�j@˥�@�ff@ə�@�p�@�p�@�hs@�X@�p�@�`B@���@�z�@�r�@�z�@ȓu@ȋD@�z�@��@Ǖ�@�\)@ƸR@�ff@�{@�hs@�/@Ĭ@�9X@�(�@�  @��;@Å@��y@�n�@���@��h@��h@�p�@�?}@���@�r�@�Z@� �@��;@���@�dZ@�
=@�~�@��@��h@�V@�Ĝ@�j@�r�@�b@�t�@�o@�E�@��T@���@�&�@�9X@�1'@�z�@���@�Ĝ@���@��@���@���@��9@���@���@�1@�t�@���@��y@�~�@�hs@���@�Z@��;@���@��P@�dZ@�dZ@�t�@�l�@�+@�o@�@�v�@��T@��7@�`B@�?}@��@���@���@���@���@���@���@��9@��@�I�@��m@��H@���@���@�r�@�b@��@�5?@�@��#@��7@�?}@��@���@���@�Ĝ@�Ĝ@���@���@��@�r�@�Q�@��@��@��y@��\@�ff@�M�@�=q@�@��^@�O�@��`@�j@�b@�1@���@��m@���@��H@��@���@���@��7@�?}@�7L@��@��j@�A�@��@��F@�|�@�dZ@�+@�ȴ@�E�@�J@���@��@��#@��h@���@���@��@��P@��@���@�ff@�=q@�-@�J@�33@�  @��@��D@x�@pQ�@dz�@\�D@St�@M�@C�F@:�@4��@/��@+ƨ@$�@��@C�@�T@�@+11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A�?}A�=qA�A�A�K�A�M�A�O�A�O�A�Q�A�Q�A�Q�A�S�A�S�A�S�A�S�A�Q�A�S�A�VA�XA�XA�ZA�S�A�XA�XA�XA�XA�ZA�\)A�\)A�VA�I�A�E�A�/A���AָRA�{A��TAծA�x�A�+A��`AӑhA�`BA���A��A�O�A���A��Aϰ!A��A�S�A�ȴA͙�A�n�A�A�A�bA�1'A�ffAɑhAƙ�A���A�bNAÕ�A�{A���A�p�A�G�A�M�A���A�^5A��mA�VA�^5A�ĜA��A��\A��DA�K�A�A�A��A�7LA��A�v�A�jA��RA�hsA�x�A���A�I�A�/A�t�A��+A��A�33A��A�$�A�G�AhsA}O�AudZAqC�Am\)Ai;dAe��AdVAcx�Ac�AadZA_�A^��A]�;A\��A[��AY�AW��AU�AT�ASG�ARȴAQK�ALVAJQ�AI��AH�/AHZAH{AG�mAFȴAD�+AC�
AB�AA��A@E�A?p�A>��A>z�A=�TA=�A;�PA7O�A69XA4�9A3G�A2r�A1�A0$�A/�7A.ȴA.^5A-A+&�A(n�A'�;A&�A%�
A$�!A#�A"-A ��At�A��AbNA$�A�A��A&�A �AdZA�!A�^A�A�A~�A�wA
=A=qAĜA$�A�A��A��A{A1'A{A��AXA�/A5?AA�A7LA��A�PA�A�`AjAp�A
=qA	?}A5?A�PAK�AĜAƨA��AZAl�A33AVA�A�HAĜA�!A��A�DA=qA�
AG�A �yA �HA ��A j@��
@�\)@���@�  @��@�{@�9X@��F@�C�@��y@�^5@��T@��`@��@�;d@�-@�p�@���@��@��D@�\)@�-@�@�@�A�@땁@���@�P@�C�@�n�@�E�@��@�hs@�`B@�7L@�r�@�F@�n�@�X@��/@��;@��y@���@�r�@�V@�V@�R@�n�@�/@�A�@�A�@�F@�K�@��H@⟾@��T@ᙚ@߶F@ޟ�@�E�@�V@��@�/@���@�
=@�O�@ץ�@��`@ӝ�@�C�@�M�@�5?@�{@�E�@�=q@�-@��@Ѳ-@��`@�j@Гu@мj@���@�Ĝ@�z�@�I�@��m@��y@Ώ\@Ͳ-@�%@�j@˥�@�ff@ə�@�p�@�p�@�hs@�X@�p�@�`B@���@�z�@�r�@�z�@ȓu@ȋD@�z�@��@Ǖ�@�\)@ƸR@�ff@�{@�hs@�/@Ĭ@�9X@�(�@�  @��;@Å@��y@�n�@���@��h@��h@�p�@�?}@���@�r�@�Z@� �@��;@���@�dZ@�
=@�~�@��@��h@�V@�Ĝ@�j@�r�@�b@�t�@�o@�E�@��T@���@�&�@�9X@�1'@�z�@���@�Ĝ@���@��@���@���@��9@���@���@�1@�t�@���@��y@�~�@�hs@���@�Z@��;@���@��P@�dZ@�dZ@�t�@�l�@�+@�o@�@�v�@��T@��7@�`B@�?}@��@���@���@���@���@���@���@��9@��@�I�@��m@��H@���@���@�r�@�b@��@�5?@�@��#@��7@�?}@��@���@���@�Ĝ@�Ĝ@���@���@��@�r�@�Q�@��@��@��y@��\@�ff@�M�@�=q@�@��^@�O�@��`@�j@�b@�1@���@��m@���@��H@��@���@���@��7@�?}@�7L@��@��j@�A�@��@��F@�|�@�dZ@�+@�ȴ@�E�@�J@���@��@��#@��h@���@���@��@��P@��@���@�ff@�=q@�-G�O�@�33@�  @��@��D@x�@pQ�@dz�@\�D@St�@M�@C�F@:�@4��@/��@+ƨ@$�@��@C�@�T@�@+11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
ȴB
��B
��B
ɺB
ɺB
ɺB
��B
ɺB
ɺB
ɺB
ɺB
ɺB
ɺB
ɺB
��B
��B
��B
��B
ɺB
ɺB
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
ɺB
ȴB
��B
�)B
�;B
�5B
�5B
�#B
��B
ĜB
��B
�}B
��B
ƨB
��B
�}B
�XB
�3B
�^B
��B
��B
�
B
�#B
�5B
��B�B-B?}B�1B��B�!B�dB��B�fB�fB��B��B��BJBPBB��B�NB��B�?B��B��B��B��B�B��By�BH�B9XB�B{BbBB
��B
�B
ǮB
��B
o�B
C�B
(�B
�B
 �B
!�B	�B	�B	�}B	�3B	��B	�{B	�PB	�PB	�1B	x�B	u�B	s�B	m�B	cTB	VB	F�B	<jB	5?B	-B	+B	!�B	�B	
=B	%B	B	B	  B��B��B��B��B��B��B��B�B�B�B�B�sB�TB�)B�B��B��B��B��BɺBȴBƨBŢB��B�}B�qB�dB�dB�dB�XB�RB�XB�dB�}BBŢBƨBƨBǮBɺB��B��B��B��B��B��B�B�BB�`B�mB�BB�/B�HB�B��B��B	  B	
=B	bB	oB	�B	�B	�B	�B	�B	!�B	'�B	(�B	)�B	+B	.B	2-B	5?B	7LB	7LB	7LB	8RB	8RB	6FB	5?B	8RB	9XB	:^B	:^B	:^B	:^B	;dB	;dB	<jB	>wB	A�B	I�B	P�B	P�B	Q�B	VB	XB	ZB	ZB	^5B	cTB	ffB	gmB	hsB	hsB	iyB	k�B	k�B	k�B	n�B	q�B	s�B	w�B	y�B	|�B	�B	�B	�%B	�+B	�+B	�+B	�%B	�hB	�{B	�uB	�uB	��B	��B	�qB	ŢB	ȴB	��B	��B	��B	ȴB	ĜB	�}B	�^B	�LB	�LB	�jB	ɺB	��B	�B	�B	��B	�B	�B	�B	�B	�B	�B	�
B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ĜB	ŢB	ƨB	ȴB	��B	��B	��B	��B	�B	�
B	�
B	��B	�B	�B	�B	�B	�)B	�/B	�/B	�/B	�#B	�#B	�B	�B	�B	�
B	��B	��B	��B	��B	��B	�B	�
B	�B	�
B	�
B	�
B	�B	�B	�B	�#B	�B	�B	�B	�B	�B	�)B	�)B	�#B	�#B	�#B	�#B	�)B	�)B	�/B	�/B	�/B	�/B	�5B	�5B	�5B	�5B	�5B	�;B	�;B	�;B	�BB	�BB	�BB	�BB	�HB	�HB	�HB	�NB	�NB	�TB	�ZB	�ZB	�NB	�HB	�HB	�BB	�BB	�BB	�NB	�mB	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
  B
  B
B
  B
  B
  B
B
B
B
B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
B
B
B
B
B
B
%B
B
B
B
%B
+B
+B
1B
	7B

=B

=B

=B

=B
DB
VB
{B
 �B
$�B
/B
33B
8RB
<jB
C�B
H�B
O�B
ZB
^5B
bNB
ffB
k�B
q�B
s�B
v�B
y�B
z�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B
ȐB
ʛB
ʜB
ɖB
ɕB
ɓB
ʛB
ɖB
ɕB
ɖB
ɕB
ɖB
ɕB
ɓB
ʛB
ʚB
ʞB
ʛB
ɕB
ɒB
ʛB
ʛB
ʞB
ʛB
ʜB
ʜB
ʛB
ʜB
ʚB
ʚB
ɕB
ȑB
��B
�B
�B
�B
�B
��B
��B
�wB
�_B
�VB
̥B
ƂB
�dB
�WB
�2B
�B
�:B
ϹB
��B
��B
��B
�B
��BfB,�B?SB�B��B��B�:BʕB�:B�;B��B��B��BB$B�B��B�BзB�B��B�jB�tB��B��B��By�BH�B9)BtBJB2B�B
�B
��B
�B
�sB
opB
CiB
(�B
�B
 �B
!�B	�B	��B	�QB	�B	��B	�PB	�&B	�&B	�B	x�B	u�B	s�B	mhB	c+B	U�B	FB	<@B	5B	,�B	*�B	!�B	XB	
B	�B	�B	 �B��B��B��B��B��B��B��B��B�B�B�xB�aB�LB�.B�B��B��B��BαBˠBɒBȌBƀB�yB�dB�WB�GB�;B�=B�=B�/B�+B�0B�:B�TB�fB�yB�BƀBǄBɑB˜BͪBήB̣BίB��B��B�B�4B�BB�B�B�B�tB��B��B��B	
B	6B	BB	SB	oB	B	�B	�B	!�B	'�B	(�B	)�B	*�B	-�B	1�B	5B	7B	7B	7B	8"B	8"B	6B	5B	8 B	9'B	:.B	:.B	:/B	:+B	;4B	;4B	<8B	>GB	AYB	I�B	P�B	P�B	Q�B	U�B	W�B	Y�B	Y�B	^B	c#B	f3B	g8B	h@B	h@B	iFB	kRB	kRB	kPB	neB	quB	s�B	w�B	y�B	|�B	��B	��B	��B	��B	��B	��B	��B	�/B	�IB	�>B	�@B	�qB	��B	�<B	�kB	�}B	ʋB	ˑB	ˎB	�|B	�eB	�FB	�'B	�B	�B	�4B	ɄB	ЮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ӿB	ѴB	ѶB	��B	��B	һB	ѴB	��B	ЮB	ˍB	�cB	�iB	�mB	�{B	ˌB	͝B	ЭB	ӾB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�	B	�B	�
B	�
B	�B	�B	�B	�B	�B	�B	� B	� B	�B	�B	�B	�B	�
B	�
B	�B	�5B	�IB	�YB	�bB	�pB	�nB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B	��B	��B	��B
 �B
 �B
 �B
 �B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B

 B

B

B

G�O�B
B
<B
 �B
$�B
.�B
2�B
8B
<-B
CZB
HyB
O�B
Y�B
]�B
bB
f)B
kFB
qmB
syB
v�B
y�B
z�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.51 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451302016080714513020160807145130  AO  ARCAADJP                                                                    20160624021730    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160624021730  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160624021730  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145130  IP                  G�O�G�O�G�O�                