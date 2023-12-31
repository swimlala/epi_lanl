CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2016-07-25T18:02:17Z AOML 3.0 creation; 2016-08-07T21:51:31Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20160725180217  20160825183417  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  5287_9017_135                   2C  D   APEX                            6529                            072314                          846 @׾��(�1   @׾����@0���
=q�d��1'1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�ffB���B�  B�  B���B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Co�fCr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C��C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dy` D��D�@ D�vfD���D�3D�33D�|�D��fD��D�6fD�y�D��3D�3D�I�Dڐ D�� D��D�<�D�p D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@�Q�A(�A((�AH(�Ah(�A�{A�{A�{A�{A�{A�{A�{A�{B
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
=B�B�B�B�B�B�B�B�B�B�B�B�B�8RB�B�B�B�B�B�B�B�B�B�k�B���B�B�B���B�B�B�B�B�C ��C��C��C��C��C
��C��C��C��C��C��C��C��C��C��C��C ��C"��C$��C&��C(��C*��C,��C.��C0��C2��C4��C6��C8��C:��C<��C>��C@��CB��CD��CF��CH��CJ��CL��CN��CP��CR��CT��CV��CX��CZ��C\��C^��C`��Cb��Cd��Cf��Ch��Cj��Cl��Cn��Cph�Cr��Ct��Cv��Cx��Cz��C|��C~��C�AHC�AHC�AHC�NC�AHC�4{C�4{C�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�NC�NC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�NC�AHC�AHC�AHC�AHC�AHC�NC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHD  �D ��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D	 �D	��D
 �D
��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D  �D ��D! �D!��D" �D"��D# �D#��D$ �D$��D% �D%��D& �D&��D' �D'��D( �D(��D) �D)��D* �D*��D+ �D+��D, �D,��D- �D-��D. �D.��D/ �D/��D0 �D0��D1 �D1��D2 �D2��D3 �D3��D4 �D4��D5 �D5��D6 �D6��D7 �D7��D8 �D8��D9 �D9��D: �D:��D; �D;��D< �D<��D= �D=��D> �D>��D? �D?��D@ �D@��DA �DA��DB �DB��DC �DC��DD �DD��DE �DE��DF �DF��DG �DG��DH �DH��DI �DI��DJ �DJ��DK �DK��DL �DL��DM �DM��DN �DN��DO �DO��DP �DP��DQ �DQ��DR �DR��DS �DS��DT �DT��DU �DU��DV �DV��DW �DW��DX �DX��DY �DY��DZ �DZ��D[ �D[��D\ �D\��D] �D]��D^ �D^��D_ �D_��D` �D`��Da �Da��Db �Db��Dc �Dc��Dd �Dd��De �De��Df �Df��Dg �Dg��Dh �Dh��Di �Di��Dj �Dj��Dk �Dk��Dl �Dl��Dm �Dm��Dn �Dn��Do �Do��Dp �Dp��Dq �Dq��Dr �Dr��Ds �Ds��Dt �Dt��Dy��D�)�D�PRD���D��D�#�D�C�D��D�ָD�D�F�D���D��D��D�Y�DڠRD��RD�D�MD�RD��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�9XA�?}A�$�A�
=A���A��;A߮A�/A��A޾wAޣ�Aއ+Aމ7Aޏ\Aޕ�Aޣ�A޴9Aޝ�Aޏ\A�~�A�p�A�\)A�7LA�
=A�%A�  A���A�jAܩ�A�5?A�1'A�JAۥ�A�I�Aڣ�A�v�A�JAؼjA�dZA־wA՟�A���A�v�A���Aң�Aҗ�A�jA�ȴA� �A��mA̬A���A�33A�JAǃA��mA�dZA�bA£�A¸RA��A�-A�-A���A��PA���A���A���A���A�p�A��FA�=qA��A��A�A�(�A���A�G�A��PA�+A�z�A��A��`A�`BA�A�A��;A��\A�9XA�v�A�A��#A�1'A�;dA�A���A�$�A�l�A�1A�XA�
=A��HA�ZA�%A�VAC�A{��Ay��AwƨAu�;At1'As��As33Ap�jAm�FAl$�Ahn�Ad�Ac�hAb�!Aa��A]��A[C�AZ��AYS�AX(�AV�+ATv�AQ33ANȴAI�FAFffAC�A@��A>JA<-A:v�A8�9A8E�A8  A6�A4�A2A1
=A0�+A/x�A-�-A,�`A+��A++A*��A(ĜA'�A%�TA%x�A%`BA%XA$�!A$A#l�A#%A"�!A ��AK�A��A��A�A33AJA/AK�AjA�A�PA�hA�A|�AG�A�A�A{A��A�yA��A��A�RA$�A��A�jAr�Az�An�AM�A�#AO�A	��A	`BA�AZAp�A�AjA$�A��A�hAt�AK�A��AbNA�AƨA�PA �/A ��A -A 1'A �`A �A -@�o@�hs@��u@��w@�|�@�p�@���@��9@���@�`B@�V@��@�b@�F@�S�@��#@���@�@�"�@�~�@�@�@�+@��T@�M�@�V@�@��@�/@�Ĝ@�Z@�@�t�@��y@�5?@�=q@�^5@�+@�~�@�M�@�{@��@웦@�r�@� �@��;@땁@�t�@�S�@�C�@�"�@�o@���@���@�n�@��@�h@�&�@���@���@�@�z�@�I�@� �@���@�l�@�R@�n�@�^5@�J@�@�I�@��;@�;d@�-@�h@�&�@�9@�r�@�A�@�b@ߕ�@�
=@���@�=q@��`@ܓu@܋D@� �@�  @۝�@�C�@��y@�^5@ٲ-@���@���@ؓu@�Z@��@�1@�  @ׅ@֗�@�X@���@�bN@Ӿw@�t�@�;d@���@�V@���@�p�@�%@��`@�Ĝ@�I�@�dZ@�"�@ΰ!@�=q@��#@ͺ^@�O�@���@̼j@�r�@�Q�@�I�@�9X@�  @�ƨ@˥�@�l�@�;d@��@���@���@���@ʰ!@ʗ�@�^5@��@�`B@ȣ�@�Q�@���@�+@�^5@�@�x�@�&�@���@�r�@� �@��@�ƨ@ÍP@�33@���@�@�M�@���@��@��@��@�I�@� �@��w@�S�@�
=@���@��+@�v�@���@��m@��\@�^5@���@�@�hs@�7L@���@��/@���@��m@�t�@�t�@�"�@��@��@���@�=q@��7@���@���@��@��
@��@�33@��\@��#@�hs@��@��u@�1@��w@�\)@���@��H@��H@��R@�{@���@�?}@��@���@��@�(�@�ƨ@�t�@�K�@�;d@�+@�
=@��y@��!@��+@��+@�5?@�$�@�{@���@��7@�7L@���@���@�9X@��m@�K�@���@��\@�~�@�E�@��@��#@��^@��-@��7@�7L@���@���@�z�@�1'@�1@��@���@�t�@�\)@�+@�^5@��#@��h@�G�@�/@�V@�Ĝ@�A�@��
@��@�ff@��y@���@�Q�@}�T@t1@j�!@b��@Xb@Pr�@HĜ@@Q�@9�#@4Z@,�@'��@ r�@@�@G�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A�9XA�?}A�$�A�
=A���A��;A߮A�/A��A޾wAޣ�Aއ+Aމ7Aޏ\Aޕ�Aޣ�A޴9Aޝ�Aޏ\A�~�A�p�A�\)A�7LA�
=A�%A�  A���A�jAܩ�A�5?A�1'A�JAۥ�A�I�Aڣ�A�v�A�JAؼjA�dZA־wA՟�A���A�v�A���Aң�Aҗ�A�jA�ȴA� �A��mA̬A���A�33A�JAǃA��mA�dZA�bA£�A¸RA��A�-A�-A���A��PA���A���A���A���A�p�A��FA�=qA��A��A�A�(�A���A�G�A��PA�+A�z�A��A��`A�`BA�A�A��;A��\A�9XA�v�A�A��#A�1'A�;dA�A���A�$�A�l�A�1A�XA�
=A��HA�ZA�%A�VAC�A{��Ay��AwƨAu�;At1'As��As33Ap�jAm�FAl$�Ahn�Ad�Ac�hAb�!Aa��A]��A[C�AZ��AYS�AX(�AV�+ATv�AQ33ANȴAI�FAFffAC�A@��A>JA<-A:v�A8�9A8E�A8  A6�A4�A2A1
=A0�+A/x�A-�-A,�`A+��A++A*��A(ĜA'�A%�TA%x�A%`BA%XA$�!A$A#l�A#%A"�!A ��AK�A��A��A�A33AJA/AK�AjA�A�PA�hA�A|�AG�A�A�A{A��A�yA��A��A�RA$�A��A�jAr�Az�An�AM�A�#AO�A	��A	`BA�AZAp�A�AjA$�A��A�hAt�AK�A��AbNA�AƨA�PA �/A ��A -A 1'A �`A �A -@�o@�hs@��u@��w@�|�@�p�@���@��9@���@�`B@�V@��@�b@�F@�S�@��#@���@�@�"�@�~�@�@�@�+@��T@�M�@�V@�@��@�/@�Ĝ@�Z@�@�t�@��y@�5?@�=q@�^5@�+@�~�@�M�@�{@��@웦@�r�@� �@��;@땁@�t�@�S�@�C�@�"�@�o@���@���@�n�@��@�h@�&�@���@���@�@�z�@�I�@� �@���@�l�@�R@�n�@�^5@�J@�@�I�@��;@�;d@�-@�h@�&�@�9@�r�@�A�@�b@ߕ�@�
=@���@�=q@��`@ܓu@܋D@� �@�  @۝�@�C�@��y@�^5@ٲ-@���@���@ؓu@�Z@��@�1@�  @ׅ@֗�@�X@���@�bN@Ӿw@�t�@�;d@���@�V@���@�p�@�%@��`@�Ĝ@�I�@�dZ@�"�@ΰ!@�=q@��#@ͺ^@�O�@���@̼j@�r�@�Q�@�I�@�9X@�  @�ƨ@˥�@�l�@�;d@��@���@���@���@ʰ!@ʗ�@�^5@��@�`B@ȣ�@�Q�@���@�+@�^5@�@�x�@�&�@���@�r�@� �@��@�ƨ@ÍP@�33@���@�@�M�@���@��@��@��@�I�@� �@��w@�S�@�
=@���@��+@�v�@���@��m@��\@�^5@���@�@�hs@�7L@���@��/@���@��m@�t�@�t�@�"�@��@��@���@�=q@��7@���@���@��@��
@��@�33@��\@��#@�hs@��@��u@�1@��w@�\)@���@��H@��H@��R@�{@���@�?}@��@���@��@�(�@�ƨ@�t�@�K�@�;d@�+@�
=@��y@��!@��+@��+@�5?@�$�@�{@���@��7@�7L@���@���@�9X@��m@�K�@���@��\@�~�@�E�@��@��#@��^@��-@��7@�7L@���@���@�z�@�1'@�1@��@���@�t�@�\)@�+@�^5@��#@��h@�G�@�/@�V@�Ĝ@�A�G�O�@��@�ff@��y@���@�Q�@}�T@t1@j�!@b��@Xb@Pr�@HĜ@@Q�@9�#@4Z@,�@'��@ r�@@�@G�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
ǮB
ȴB
��B
��B
�B
�
B
�B
�BB
�ZB
�fB
�fB
�fB
�mB
�B
�B
��B
��B%B
=BVBoB�B!�B%�B%�B%�B(�B.B2-B;dBA�BB�BB�BI�B]/Bt�B�B�B}�By�Bt�B{�B� B�%B�DB�VB�hB�B��B��B�RB�HB�ZB�`B�NB�TB�`B�sB��B��B%BN�BiyBhsBy�B�%B�+B��B��B��B��B��B�uB�bBu�BR�BJ�BJ�BG�B<jB#�B	7B��B��B�B�B��BǮB�RB��B|�Bt�Bl�Bk�Bx�BW
B
��B
B
�JB
S�B
D�B
?}B
5?B
)�B
�B
B	��B	�B	�TB	�/B	�#B	�B	��B	�}B	�LB	��B	��B	�hB	�PB	�1B	z�B	p�B	n�B	hsB	aHB	YB	I�B	6FB	)�B	oB	DB��B�B�mB�fB�mB�mB�fB�ZB�HB�B��B��B��B�
B�
B�NB�sB�B�B�TB�ZB�sB�B��B��B��B��B��B��B��B��B�B��B	B	hB	�B	5?B	M�B	cTB	ffB	aHB	\)B	Q�B	I�B	G�B	G�B	F�B	A�B	<jB	49B	+B	-B	33B	;dB	G�B	J�B	K�B	M�B	O�B	P�B	P�B	O�B	O�B	G�B	C�B	7LB	2-B	0!B	.B	.B	-B	,B	,B	0!B	6FB	5?B	7LB	:^B	9XB	9XB	7LB	7LB	49B	8RB	E�B	J�B	I�B	F�B	A�B	>wB	<jB	<jB	7LB	49B	49B	5?B	6FB	6FB	7LB	>wB	C�B	F�B	I�B	\)B	ffB	ffB	hsB	r�B	�B	�hB	��B	��B	��B	�B	�B	�B	�B	�'B	�-B	�-B	�FB	�^B	�dB	�jB	�wB	B	B	ĜB	ŢB	ŢB	ŢB	ŢB	ŢB	ƨB	ƨB	ǮB	ǮB	ǮB	ǮB	ǮB	ȴB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�#B	�5B	�BB	�BB	�BB	�HB	�HB	�HB	�HB	�NB	�TB	�TB	�fB	�mB	�mB	�sB	�sB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	��B	��B	��B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
%B
%B
+B
+B
1B
+B
+B
+B
1B
1B
	7B

=B
DB
DB
JB
PB
PB
PB
PB
PB
PB
JB
JB
JB
PB
JB
JB
JB
PB
JB
PB
PB
PB
PB
PB
PB
PB
PB
\B
JB
\B
{B
�B
!�B
(�B
/B
49B
9XB
?}B
D�B
J�B
R�B
YB
]/B
aHB
ffB
l�B
p�B
u�B
y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B
ǊB
ȐB
��B
��B
��B
��B
��B
�B
�6B
�CB
�BB
�CB
�GB
�_B
�lB
��B
��BB
B/BIBtB!�B%�B%�B%�B(�B-�B2B;>BAaBBlBBjBI�B]Bt�B��B��B}�By�Bt�B{�B�B��B�B�/B�?B��B��B��B�'B�B�0B�6B�%B�(B�7B�HB��B��B�BN�BiLBhIBy�B��B��B�VB�vB�bB�bB�_B�KB�8Bu�BR�BJ�BJ�BGB<>B#�B	B��B��B�ZB��BϭB�~B�#B�WB|�Bt�Bl\BkXBx�BV�B
��B
�^B
�B
S�B
DmB
?QB
5B
)�B
^B
�B	��B	�[B	�(B	�B	��B	��B	άB	�QB	�B	��B	�iB	�<B	�&B	�B	z�B	p{B	npB	hKB	a B	X�B	I�B	6B	)�B	IB	B��B�vB�FB�?B�GB�FB�=B�3B� B��B��B��B��B��B��B�(B�IB�pB�nB�+B�2B�IB�zB��B��B��B��B��B��B��B��B�B��B	�B	<B	�B	5B	M�B	c#B	f4B	aB	[�B	Q�B	I�B	G�B	G�B	FxB	A[B	<<B	4B	*�B	,�B	3B	;3B	G}B	J�B	K�B	M�B	O�B	P�B	P�B	O�B	O�B	GB	CeB	7B	1�B	/�B	-�B	-�B	,�B	+�B	+�B	/�B	6B	5B	7B	:.B	9%B	9'B	7B	7B	4B	8"B	ErB	J�B	I�B	FvB	AZB	>CB	<8B	<9B	7B	4	B	4B	5
B	6B	6B	7B	>DB	CbB	FwB	I�B	[�B	f2B	f1B	h@B	rzB	��B	�4B	�NB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�)B	�/B	�5B	�CB	�WB	�XB	�eB	�iB	�kB	�jB	�jB	�jB	�qB	�qB	�xB	�wB	�wB	�zB	�vB	�}B	�~B	�{B	ʊB	ːB	̓B	̖B	͛B	͛B	ΣB	΢B	ϨB	ϧB	ѴB	ҺB	ҽB	һB	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�B	�B	�B	�B	�B	�-B	�5B	�5B	�:B	�;B	�:B	�:B	�CB	�?B	�EB	�EB	�EB	�MB	�NB	�NB	�JB	�JB	�LB	�CB	�OB	�ZB	�YB	�ZB	�YB	�YB	�_B	�YB	�bB	�_B	�_B	�aB	�`B	�cB	�mB	�nB	�oB	�pB	�rB	�pB	�oB	�vB	�uB	�}B	�}B	�}B	�~B	�}B	�~B	�B	�}B	�|B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�{B	�tB	�~B	�|B	�B	�B	�B	�}B	�|B	�vB	�sB	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B

B
	B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
G�O�B
B
!B
?B
VB
!�B
(�B
.�B
3�B
9B
?>B
D_B
J�B
R�B
X�B
\�B
aB
f(B
lKB
pgB
u�B
y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.51 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451312016080714513120160807145131  AO  ARCAADJP                                                                    20160725180217    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160725180217  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160725180217  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145131  IP                  G�O�G�O�G�O�                