CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-02-26T22:14:39Z AOML 3.0 creation; 2016-08-07T21:51:14Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20150226221439  20160807145114  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               #A   AO  5287_9017_035                   2C  D   APEX                            6529                            072314                          846 @�:I�
�1   @�:Jb�� @1����+�d�G�z�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    #A   B   B   @�33@�  A   A   A@  A`  A�  A�33A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�ffB���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH�CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt� Dyl�D��D�VfD�vfD��3D�  D�9�D�s3D��fD��D�FfD��3D�� D�  D�FfD�l�D�� D���D�0 D�3D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@�Q�A(�A((�AH(�Ah(�A�{A�G�A�{A�{A�{A�{A�{A�{B
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
=B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�8RB�k�B���B�B�B�B�B�B�B�B�B�B�C ��C��C��C��C��C
��C��C��C��C��C��C��C��C��C��C��C ��C"��C$��C&��C(��C*��C,��C.��C0��C2��C4��C6��C8��C:��C<��C>��C@��CB��CD��CF��CH�)CJ��CL��CN��CP��CR��CT��CV��CX��CZ��C\��C^��C`��Cb��Cd��Cf��Ch��Cj��Cl��Cn��Cp��Cr��Ct��Cv��Cx��Cz��C|��C~��C�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�NC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHD  �D ��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D	 �D	��D
 �D
��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D  �D ��D! �D!��D" �D"��D# �D#��D$ �D$��D% �D%��D& �D&��D' �D'��D( �D(��D) �D)��D* �D*��D+ �D+��D, �D,��D- �D-��D. �D.��D/ �D/��D0 �D0��D1 �D1��D2 �D2��D3 �D3��D4 �D4��D5 �D5��D6 �D6��D7 �D7��D8 �D8��D9 �D9��D: �D:��D; �D;��D< �D<��D= �D=��D> �D>��D? �D?��D@ �D@��DA �DA��DB �DB��DC �DC��DD �DD��DE �DE��DF �DF��DG �DG��DH �DH��DI �DI��DJ �DJ��DK �DK��DL �DL��DM �DM��DN �DN��DO �DO��DP �DP��DQ �DQ��DR �DR��DS �DS��DT �DT��DU �DU��DV �DV��DW �DW��DX �DX��DY �DY��DZ �DZ��D[ �D[��D\ �D\��D] �D]��D^ �D^��D_ �D_��D` �D`��Da �Da��Db �Db��Dc �Dc��Dd �Dd��De �De��Df �Df��Dg �Dg��Dh �Dh��Di �Di��Dj �Dj��Dk �Dk��Dl �Dl��Dm �Dm��Dn �Dn��Do �Do��Dp �Dp��Dq �Dq��Dr �Dr��Ds �Ds��Dt �Dt��Dt�Dy�qD�)�D�f�D���D�ӅD�RD�I�D���D�ƸD�D�V�D���D��RD�RD�V�D�}D��RD�	�D�@RD�D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�ƨA���Aʺ^Aʩ�AʬAʝ�AʍPA�hsA�5?A�%A��A��A��`A��/A��A���A���A���A���A���A���Aɲ-AɋDA�^5A�1'A���Aȥ�A�~�A�l�A��`A�|�AőhA���Aĕ�A�~�A�ffA�A�A�AøRA×�A�n�A�1'A��HAA�dZA�-A�  A��9A�~�A�M�A�K�A��9A��-A��A�t�A���A���A���A��A��9A�A��7A���A�bA�A�A�A�hsA���A��A��A���A��TA��jA�-A��HA�v�A�A��A��A�+A��
A��!A�hsA�(�A���A��A�{A��HA��A�O�A��FA�ȴA��A�t�A��A�"�A�|�A�Q�A�C�A��`A�n�A��HA��-A��A��`A�hsA���Ax�A|��AwoAs
=Ao�Ak�AiXAe�Aax�A`1A_XA_VA^ĜA^�uA]O�A\9XAZĜAW�#AVJAS�AR�AP1'AO�AM%AJ�`AI�AHE�AG�AF�`AE�
AA�A<�jA<Q�A;
=A9��A7�hA4��A2��A2bA1��A/��A-�FA,��A)��A&�9A%�
A#7LA!%A�!A�
A`BA��AbAG�A��AJAG�A-AS�AbNA��A�A�A  A7LAƨAv�A��A7LA�RA(�AA�AC�A{A�;A��Ax�AG�AoA
�/A
�RA
v�A	��A�A�A�wA|�A|�AC�A�A��Ar�A9XA�7A��A��A�A�A$�A^5A(�At�A�HAr�A�FA?}A�A �`A ��A ��A �+A A�A �A {@���@���@���@�b@�t�@���@�I�@�|�@���@�^5@�{@��-@�D@�|�@�+@�O�@�Q�@�t�@�@�J@�@�V@�9X@�b@�  @�F@�+@�V@�7@�r�@旍@��@��@�@�j@��@�
=@⟾@�J@�G�@���@�j@�Z@�1@�1@�1@߾w@�;d@���@ݺ^@�hs@��@ܛ�@�(�@�l�@�
=@ڗ�@��@ٲ-@�p�@�O�@���@�1'@ְ!@�5?@��@ա�@�p�@�/@�I�@�33@�"�@���@��T@�hs@���@��@�33@�ȴ@Χ�@�v�@�n�@�V@��#@���@�1'@�|�@�33@�5?@Ɂ@�?}@��@Ȭ@�r�@�j@�Z@�I�@�A�@�9X@�  @ǶF@�t�@�+@��y@�M�@�7L@ċD@�1'@��@þw@�|�@�\)@�@�~�@�V@�{@�X@�A�@���@���@�7L@�I�@���@���@�5?@��@��-@���@���@��@��/@�Z@��F@�|�@�|�@��P@�\)@���@�?}@��@��@�Z@�ƨ@���@�l�@�"�@�^5@���@��7@��@� �@��m@��
@��F@�|�@�\)@��@���@�5?@�hs@���@��@� �@��
@�l�@�o@��R@�$�@�{@�@��T@���@�hs@�`B@�X@��@��j@��u@���@�"�@��@��y@�V@���@��@�X@���@��@�ƨ@��@�S�@�@�^5@��#@�@��^@��@���@�|�@�
=@��+@���@��@�z�@��@�  @��P@�;d@��!@�n�@�J@���@�@���@�p�@��@��/@���@�r�@�A�@� �@�1@��;@�C�@�v�@�$�@�J@��@��@���@��@�X@�7L@���@��9@�Z@�(�@��@��w@��@�K�@�"�@�@��R@�~�@�E�@�@���@�@���@�x�@�O�@���@��@�9X@��
@�|�@�"�@��@�n�@�@��@�`B@��`@��j@��u@�bN@�b@���@���@}@x��@qX@g��@ax�@Z��@P��@I&�@A�#@;o@5��@0A�@+@%O�@�;@C�@�w@�@�;111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�ƨA���Aʺ^Aʩ�AʬAʝ�AʍPA�hsA�5?A�%A��A��A��`A��/A��A���A���A���A���A���A���Aɲ-AɋDA�^5A�1'A���Aȥ�A�~�A�l�A��`A�|�AőhA���Aĕ�A�~�A�ffA�A�A�AøRA×�A�n�A�1'A��HAA�dZA�-A�  A��9A�~�A�M�A�K�A��9A��-A��A�t�A���A���A���A��A��9A�A��7A���A�bA�A�A�A�hsA���A��A��A���A��TA��jA�-A��HA�v�A�A��A��A�+A��
A��!A�hsA�(�A���A��A�{A��HA��A�O�A��FA�ȴA��A�t�A��A�"�A�|�A�Q�A�C�A��`A�n�A��HA��-A��A��`A�hsA���Ax�A|��AwoAs
=Ao�Ak�AiXAe�Aax�A`1A_XA_VA^ĜA^�uA]O�A\9XAZĜAW�#AVJAS�AR�AP1'AO�AM%AJ�`AI�AHE�AG�AF�`AE�
AA�A<�jA<Q�A;
=A9��A7�hA4��A2��A2bA1��A/��A-�FA,��A)��A&�9A%�
A#7LA!%A�!A�
A`BA��AbAG�A��AJAG�A-AS�AbNA��A�A�A  A7LAƨAv�A��A7LA�RA(�AA�AC�A{A�;A��Ax�AG�AoA
�/A
�RA
v�A	��A�A�A�wA|�A|�AC�A�A��Ar�A9XA�7A��A��A�A�A$�A^5A(�At�A�HAr�A�FA?}A�A �`A ��A ��A �+A A�A �A {@���@���@���@�b@�t�@���@�I�@�|�@���@�^5@�{@��-@�D@�|�@�+@�O�@�Q�@�t�@�@�J@�@�V@�9X@�b@�  @�F@�+@�V@�7@�r�@旍@��@��@�@�j@��@�
=@⟾@�J@�G�@���@�j@�Z@�1@�1@�1@߾w@�;d@���@ݺ^@�hs@��@ܛ�@�(�@�l�@�
=@ڗ�@��@ٲ-@�p�@�O�@���@�1'@ְ!@�5?@��@ա�@�p�@�/@�I�@�33@�"�@���@��T@�hs@���@��@�33@�ȴ@Χ�@�v�@�n�@�V@��#@���@�1'@�|�@�33@�5?@Ɂ@�?}@��@Ȭ@�r�@�j@�Z@�I�@�A�@�9X@�  @ǶF@�t�@�+@��y@�M�@�7L@ċD@�1'@��@þw@�|�@�\)@�@�~�@�V@�{@�X@�A�@���@���@�7L@�I�@���@���@�5?@��@��-@���@���@��@��/@�Z@��F@�|�@�|�@��P@�\)@���@�?}@��@��@�Z@�ƨ@���@�l�@�"�@�^5@���@��7@��@� �@��m@��
@��F@�|�@�\)@��@���@�5?@�hs@���@��@� �@��
@�l�@�o@��R@�$�@�{@�@��T@���@�hs@�`B@�X@��@��j@��u@���@�"�@��@��y@�V@���@��@�X@���@��@�ƨ@��@�S�@�@�^5@��#@�@��^@��@���@�|�@�
=@��+@���@��@�z�@��@�  @��P@�;d@��!@�n�@�J@���@�@���@�p�@��@��/@���@�r�@�A�@� �@�1@��;@�C�@�v�@�$�@�J@��@��@���@��@�X@�7L@���@��9@�Z@�(�@��@��w@��@�K�@�"�@�@��R@�~�@�E�@�@���@�@���@�x�@�O�@���@��@�9X@��
@�|�@�"�@��@�n�@�@��@�`B@��`@��j@��u@�bNG�O�@���@���@}@x��@qX@g��@ax�@Z��@P��@I&�@A�#@;o@5��@0A�@+@%O�@�;@C�@�w@�@�;111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�'B�'B�'B�!B�!B�!B�B�B�B�B�B�B�B�B�B�B�B�!B�3B�XB�dBÖB��B�yB��B!�B+B1'B/B�B{B��B�B��B  BB%B+BBB+B\B�B-B2-B5?B33B1'B2-B=qBH�BP�Bq�Bp�B^5BJ�BA�B9XB)�B!�B�BoB\BB��B�B�B�;B�
B��BǮB�dB�B��B��B�DB�Bx�Bm�BjBffBdZB`BB[#BP�B:^B%B�B�#B��B��B�B��Bz�Bk�BcTBT�B/B�BhB%B
ȴB
�'B
��B
�B
{�B
l�B
]/B
F�B
�B
B	�B	�
B	ŢB	�B	��B	�{B	�bB	�VB	�JB	�=B	�B	x�B	o�B	bNB	ZB	P�B	H�B	?}B	:^B	0!B	&�B	!�B	�B	�B	uB	DB��B�yB�fB�BB�#B��B��BĜBB��B�^B�?B�'B�B��B��B��B��B��B��B��B��B��B�B�LB��B��B��B��B��B��B��B��B��B�
B�B�)B�)B�/B�5B�;B�ZB�HB�5B�TB�sB�yB�B�B�B��B	B��B��B	
=B	oB	{B	uB	oB	\B	�B	&�B	)�B	+B	+B	.B	2-B	0!B	;dB	C�B	D�B	I�B	L�B	K�B	M�B	M�B	L�B	L�B	L�B	L�B	L�B	L�B	M�B	N�B	R�B	VB	XB	YB	[#B	\)B	^5B	^5B	_;B	_;B	`BB	aHB	cTB	cTB	dZB	ffB	iyB	k�B	l�B	n�B	p�B	q�B	s�B	t�B	t�B	t�B	v�B	x�B	y�B	|�B	�B	�B	�B	�B	�1B	�DB	�VB	�\B	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�3B	�3B	�9B	�?B	�?B	�RB	�XB	�XB	�XB	�^B	�^B	�dB	�qB	�}B	��B	B	B	B	B	ÖB	ƨB	ƨB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�#B	�#B	�)B	�/B	�5B	�;B	�BB	�BB	�;B	�;B	�/B	�#B	�/B	�/B	�)B	�#B	�B	�B	�B	�B	�)B	�)B	�/B	�;B	�5B	�/B	�5B	�;B	�NB	�ZB	�fB	�ZB	�sB	�yB	�sB	�yB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
+B
1B
1B
	7B
	7B

=B
	7B

=B

=B

=B

=B
DB
DB
JB
JB
JB
PB
PB
VB
VB
VB
\B
\B
\B
bB
�B
oB
�B
�B
 �B
%�B
/B
5?B
7LB
C�B
I�B
Q�B
W
B
\)B
^5B
bNB
ffB
jB
m�B
q�B
t�B
x�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�	B�0B�<B�lB��B�OB��B!�B*�B1 B.�B�BRB��B�B��B��B�B�BB�B�BB4B�B,�B2B5B3	B1B2B=IBH�BP�Bq�Bp~B^BJ�BA^B9.B)�B!�BtBBB0B�B��B�B�SB�B��BзBǂB�4B��B��B��B�B��Bx�BmbBjPBf8Bd,B`BZ�BP�B:,B�B�gB��B͡B�WB��B�QBz�BkSBc%BT�B.�B`B9B�B
ȆB
��B
�QB
��B
{�B
l[B
] B
F}B
�B
�B	�jB	��B	�uB	��B	�{B	�PB	�8B	�,B	� B	�B	��B	x�B	ouB	b#B	Y�B	P�B	H�B	?VB	:5B	/�B	&�B	!�B	wB	^B	OB	B��B�TB�>B�B��B��BʛB�xB�jB�[B�8B�B�B��B��B��B��B��B��B��B��B��B��B��B�#B˜BͩB϶BͧB˜B˜BͩBϴB��B��B��B��B��B�B�B�B�-B�B�	B�)B�GB�LB�XB�fB�qB��B	�B��B��B	
B	AB	LB	DB	=B	,B	qB	&�B	)�B	*�B	*�B	-�B	1�B	/�B	;3B	CfB	DlB	I�B	L�B	K�B	M�B	M�B	L�B	L�B	L�B	L�B	L�B	L�B	M�B	N�B	R�B	U�B	W�B	X�B	Z�B	[�B	^B	^B	_	B	_	B	`B	aB	c#B	c!B	d)B	f0B	iDB	kSB	lVB	nbB	poB	qvB	s�B	t�B	t�B	t�B	v�B	x�B	y�B	|�B	��B	��B	��B	��B	��B	�B	� B	�'B	�2B	�FB	�JB	�KB	�UB	�^B	�\B	�dB	�iB	�nB	�pB	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�B	�B	�&B	�&B	�,B	�:B	�DB	�RB	�WB	�VB	�VB	�WB	�^B	�oB	�pB	�pB	�xB	ɂB	ˋB	͛B	ϧB	ϥB	ϤB	ϥB	ϥB	ϧB	ЬB	ϥB	ѶB	ҺB	ӿB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�B	�B	�B	�-B	�"B	�8B	�>B	�9B	�?B	�:B	�8B	�9B	�BB	�CB	�GB	�DB	�IB	�QB	�MB	�NB	�KB	�LB	�QB	�JB	�LB	�PB	�UB	�_B	�bB	�eB	�eB	�dB	�jB	�jB	�iB	�iB	�kB	�hB	�iB	�fB	�fB	�gB	�oB	�oB	�uB	�{B	�yB	�{B	�}B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B

 B
�B

 B

B

B

B
B
B
B
B
B
B
B
B
B
B
B
B
 B
%G�O�B
2B
JB
yB
 �B
%�B
.�B
4�B
7B
CXB
I|B
Q�B
V�B
[�B
]�B
bB
f*B
jAB
mRB
qmB
t|B
x�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.51 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451142016080714511420160807145114  AO  ARCAADJP                                                                    20150226221439    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150226221439  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150226221439  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145115  IP                  G�O�G�O�G�O�                