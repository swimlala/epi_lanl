CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-21T12:59:51Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         (Matthew Alkire, University of Washington      @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7,   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  74   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7t   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     88   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8X   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8x   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           8|   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
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
_FillValue                    g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qt   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �\   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �\   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �P   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �$   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �(   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �,Argo profile    3.1 1.2 19500101000000  20181121125951  20190405100757  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  5287                            2C  D   APEX                            6529                            072314                          846 @��`�;��1   @��aK�#|@1�/���dV��O�;1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C�C  C	��C�fC  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CM�fCP  CR  CT  CV  CW�fCZ  C\  C^  C`  Cb�Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dts3Dy��D�fD�VfD���D��fD�	�D�L�D�vfD���D��D�S3D���D�ٚD�  D�I�D�y�D� D�  D�,�D�ffD���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�z�@�G�A��A(��AH��Ah��A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�B(�B
(�B(�B(�B"(�B*(�B2(�B:(�BB(�BJ(�BR(�BZ(�Bb(�Bj(�Br(�Bz(�B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{C �=C�=C�=C��C�=C
W
Cp�C�=C�=C�=C�=C�=C�=C�=C�=C�=C �=C"�=C$�=C&�=C(�=C*�=C,�=C.�=C0�=C2�=C4�=C6�=C8�=C:�=C<�=C>�=C@�=CB�=CD�=CF�=CH�=CJ�=CL�=CNp�CP�=CR�=CT�=CV�=CXp�CZ�=C\�=C^�=C`�=Cb��Cd�=Cf�=Ch�=Cj�=Cl�=Cn�=Cp�=Cr�=Ct�=Cv�=Cx�=Cz�=C|�=C~�=C�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�ED "�D ��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D	"�D	��D
"�D
��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D "�D ��D!"�D!��D""�D"��D#"�D#��D$"�D$��D%"�D%��D&"�D&��D'"�D'��D("�D(��D)"�D)��D*"�D*��D+"�D+��D,"�D,��D-"�D-��D."�D.��D/"�D/��D0"�D0��D1"�D1��D2"�D2��D3"�D3��D4"�D4��D5"�D5��D6"�D6��D7"�D7��D8"�D8��D9"�D9��D:"�D:��D;"�D;��D<"�D<��D="�D=��D>"�D>��D?"�D?��D@"�D@��DA"�DA��DB"�DB��DC"�DC��DD"�DD��DE"�DE��DF"�DF��DG"�DG��DH"�DH��DI"�DI��DJ"�DJ��DK"�DK��DL"�DL��DM"�DM��DN"�DN��DO"�DO��DP"�DP��DQ"�DQ��DR"�DR��DS"�DS��DT"�DT��DU"�DU��DV"�DV��DW"�DW��DX"�DX��DY"�DY��DZ"�DZ��D["�D[��D\"�D\��D]"�D]��D^"�D^��D_"�D_��D`"�D`��Da"�Da��Db"�Db��Dc"�Dc��Dd"�Dd��De"�De��Df"�Df��Dg"�Dg��Dh"�Dh��Di"�Di��Dj"�Dj��Dk"�Dk��Dl"�Dl��Dm"�Dm��Dn"�Dn��Do"�Do��Dp"�Dp��Dq"�Dq��Dr"�Dr��Ds"�Ds��Dt"�Dt��Dy�)D��D�g�D���D��D��D�^D���D��D�.D�d{D���D���D�HD�Z�Dڊ�D��HD�HD�>D�w�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��
A��#A��/A��A��A���A���A�A�1'A�33A�1'A�1'A�1'A�33A�5?A�9XA�7LA�?}A�I�A�VA�`BA�`BA�hsA�n�A�p�A�r�A�r�A�n�A�hsA�hsA�hsA�ffA�dZA�bNA�^5A�S�A�I�A�$�A�1A���A��A��/A���A���A�ȴA�AѺ^Aѩ�Aџ�AёhAѓuAёhAщ7A�l�A�JAиRA���Aϗ�A�?}A�A���A��HA�XA˕�Aʡ�Aț�A��TA�S�ADA���A�|�A�v�A�E�A�G�A��wA�S�A���A�ȴA�I�A��TA�ƨA��+A�~�A�1'A�%A�z�A���A��yA�%A��7A���A�z�A�;dA�t�A��yA��A�x�A�?}A��+A��A�ȴA�\)A��A�A��#A�+A�t�A�M�A�A�"�A}7LA{�;Ayp�Au�Am��AhQ�AeoA`r�A^ffA[�A[AZ��AX��AXjAV�jAT1AS+AP�AJffAH��AGt�AEx�AC�7AA�AAA?�A=��A<{A: �A7�mA6��A7�A8�A8A6�uA69XA5t�A4�uA3�7A2A�A0�9A/\)A.=qA-��A,��A+�A+p�A*v�A)�^A(�A(z�A(9XA'��A'"�A&��A&r�A%��A%XA$�HA$�uA$5?A#�^A#?}A#+A"�yA"��A"I�A"A!ƨA!�7A ȴA {A��A��AXA�RAZA�Al�A��AVAbA��A��AJA��A?}Az�A�A��A`BAoA�A�
A�hA`BA7LA�A�DA9XA\)A�jA��AVA�AXA/AffA�mA7LA�yA�+A=qA�hAoA�`A�uA1A��A�Ap�A;dA
��A
��A
-A
�A	��A	O�A��A��A�DAQ�AbA�7AA�`A�!AffA �A�
A�-A�Al�A;dA��AffA(�A  AƨA�#A�;A��Al�A"�A�HA�9A~�AVA9XAbA�AƨA��A|�AhsAoA ĜA �\A Q�A   @�+@�n�@��@��-@�x�@���@��
@��@�33@�$�@��^@�`B@��u@�@���@�&�@�r�@�1'@���@�C�@��@�h@��/@��
@�|�@��y@�@�u@��;@��@�@�G�@��@�@��;@�R@���@��@�\@�@���@�-@�7@�X@�Ĝ@㝲@�h@�9@��;@�|�@�o@޸R@�n�@ݩ�@�Ĝ@�Q�@�1'@۝�@���@١�@� �@���@���@���@�  @�S�@ҸR@�hs@�C�@θR@�5?@���@�Z@˝�@���@��T@�G�@��`@ȃ@�ƨ@��@�=q@�@őh@�p�@��@�j@�1'@�ƨ@�K�@��@°!@�~�@�=q@���@���@�X@�%@�z�@��@��w@��@�|�@��!@�M�@�@��-@�x�@�?}@�&�@�V@���@��@��@��
@��F@���@�S�@�ȴ@�n�@���@�X@�G�@�7L@��@���@��`@��j@�r�@�A�@�  @��;@��F@�|�@�;d@���@�{@��^@�p�@��D@�I�@�1'@��;@�K�@�"�@�ȴ@�=q@��@��7@�7L@��@���@�r�@� �@��@���@��@�dZ@�+@���@�ff@��@��@��-@�x�@�x�@�hs@�/@��@��D@�z�@�bN@�I�@�b@���@�|�@�"�@���@�~�@��@��@��h@�O�@���@�bN@�(�@��@���@��w@��@�\)@�@��y@��@��R@�~�@�ff@�{@��@���@�G�@��j@��
@��F@���@�C�@��@���@��@�|�@��T@��@���@~@v��@pb@g|�@]�@SC�@N$�@E�@>E�@8��@2�@-�T@(  @"~�@�j@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��
A��#A��/A��A��A���A���A�A�1'A�33A�1'A�1'A�1'A�33A�5?A�9XA�7LA�?}A�I�A�VA�`BA�`BA�hsA�n�A�p�A�r�A�r�A�n�A�hsA�hsA�hsA�ffA�dZA�bNA�^5A�S�A�I�A�$�A�1A���A��A��/A���A���A�ȴA�AѺ^Aѩ�Aџ�AёhAѓuAёhAщ7A�l�A�JAиRA���Aϗ�A�?}A�A���A��HA�XA˕�Aʡ�Aț�A��TA�S�ADA���A�|�A�v�A�E�A�G�A��wA�S�A���A�ȴA�I�A��TA�ƨA��+A�~�A�1'A�%A�z�A���A��yA�%A��7A���A�z�A�;dA�t�A��yA��A�x�A�?}A��+A��A�ȴA�\)A��A�A��#A�+A�t�A�M�A�A�"�A}7LA{�;Ayp�Au�Am��AhQ�AeoA`r�A^ffA[�A[AZ��AX��AXjAV�jAT1AS+AP�AJffAH��AGt�AEx�AC�7AA�AAA?�A=��A<{A: �A7�mA6��A7�A8�A8A6�uA69XA5t�A4�uA3�7A2A�A0�9A/\)A.=qA-��A,��A+�A+p�A*v�A)�^A(�A(z�A(9XA'��A'"�A&��A&r�A%��A%XA$�HA$�uA$5?A#�^A#?}A#+A"�yA"��A"I�A"A!ƨA!�7A ȴA {A��A��AXA�RAZA�Al�A��AVAbA��A��AJA��A?}Az�A�A��A`BAoA�A�
A�hA`BA7LA�A�DA9XA\)A�jA��AVA�AXA/AffA�mA7LA�yA�+A=qA�hAoA�`A�uA1A��A�Ap�A;dA
��A
��A
-A
�A	��A	O�A��A��A�DAQ�AbA�7AA�`A�!AffA �A�
A�-A�Al�A;dA��AffA(�A  AƨA�#A�;A��Al�A"�A�HA�9A~�AVA9XAbA�AƨA��A|�AhsAoA ĜA �\A Q�A   @�+@�n�@��@��-@�x�@���@��
@��@�33@�$�@��^@�`B@��u@�@���@�&�@�r�@�1'@���@�C�@��@�h@��/@��
@�|�@��y@�@�u@��;@��@�@�G�@��@�@��;@�R@���@��@�\@�@���@�-@�7@�X@�Ĝ@㝲@�h@�9@��;@�|�@�o@޸R@�n�@ݩ�@�Ĝ@�Q�@�1'@۝�@���@١�@� �@���@���@���@�  @�S�@ҸR@�hs@�C�@θR@�5?@���@�Z@˝�@���@��T@�G�@��`@ȃ@�ƨ@��@�=q@�@őh@�p�@��@�j@�1'@�ƨ@�K�@��@°!@�~�@�=q@���@���@�X@�%@�z�@��@��w@��@�|�@��!@�M�@�@��-@�x�@�?}@�&�@�V@���@��@��@��
@��F@���@�S�@�ȴ@�n�@���@�X@�G�@�7L@��@���@��`@��j@�r�@�A�@�  @��;@��F@�|�@�;d@���@�{@��^@�p�@��D@�I�@�1'@��;@�K�@�"�@�ȴ@�=q@��@��7@�7L@��@���@�r�@� �@��@���@��@�dZ@�+@���@�ff@��@��@��-@�x�@�x�@�hs@�/@��@��D@�z�@�bN@�I�@�b@���@�|�@�"�@���@�~�@��@��@��h@�O�@���@�bN@�(�@��@���@��w@��@�\)@�@��y@��@��R@�~�@�ff@�{@��@���@�G�@��j@��
@��F@���@�C�@��@���@��@�|�@��T@��@���@~@v��@pb@g|�@]�@SC�@N$�@E�@>E�@8��@2�@-�T@(  @"~�@�j@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBbBbBhB{B�B�B�B�B&�B&�B'�B'�B'�B'�B'�B(�B(�B)�B-B0!B2-B2-B49B6FB6FB6FB7LB7LB7LB7LB8RB8RB>wBA�BC�BH�BH�BJ�BJ�BJ�BK�BK�BJ�BJ�BJ�BJ�BL�BM�BL�BJ�BK�BK�BJ�BH�BL�BQ�B\)BcTBv�B�B�1B�\B�XB�^B�jB��B��B�`B�B+B>wBF�BP�BW
BT�BQ�BO�B_;B]/BYBVBO�B<jB49B7LB8RB8RB6FB33B1'B.B"�B  B��B��By�BffBJ�B\B
ÖB
}�B
bNB
2-B

=B	�fB	ƨB	�?B	�FB	ĜB	�^B	��B	�bB	�B	v�B	gmB	J�B	9XB	$�B	�B	bB	\B	PB	1B	B	  B��B��B�B�B�yB�mB�`B�`B�fB�yB�B��B��B	B	oB	"�B	R�B	k�B	�%B	�VB	�hB	��B	��B	��B	��B	�'B	�RB	��B	ĜB	��B	��B	��B	�B	�HB	�B	��B	��B	��B	��B	��B
  B
B
B
%B

=B
JB
PB
\B
\B
uB
�B
�B
�B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
{B
{B
{B
uB
{B
{B
uB
{B
uB
{B
�B
�B
{B
{B
{B
uB
uB
oB
hB
bB
\B
VB
	7B
	7B
	7B
1B
1B
DB
PB
DB

=B
	7B
+B
%B
B
B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	��B	��B	�B	�B	�B	�B	�mB	�NB	�HB	�BB	�5B	�5B	�BB	�HB	�NB	�NB	�NB	�ZB	�ZB	�ZB	�ZB	�ZB	�`B	�ZB	�ZB	�`B	�fB	�fB	�mB	�mB	�sB	�sB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
+B
1B
1B
1B
1B
1B
1B
	7B
	7B
	7B
	7B
	7B
	7B
	7B

=B

=B

=B

=B
DB
PB
VB
�B
�B
"�B
$�B
)�B
/B
5?B
>wB
B�B
F�B
G�B
L�B
S�B
XB
]/B
`BB
ffB
jB
n�B
r�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B6B6B:BRBTBbBbBgB&�B&�B'�B'�B'�B'�B'�B(�B(�B)�B,�B/�B2B2B4B6B6B6B7#B7"B7!B7!B8&B8&B>NBA^BCkBH�BH�BJ�BJ�BJ�BK�BK�BJ�BJ�BJ�BJ�BL�BM�BL�BJ�BK�BK�BJ�BH�BL�BQ�B[�Bc*Bv�B��B�B�-B�+B�2B�AB�]B��B�3BYB*�B>GBFwBP�BV�BT�BQ�BO�B_B\�BX�BU�BO�B<;B4	B7B8"B8$B6B3B0�B-�B"�B��BʓB�QBy�Bf6BJ�B)B
�bB
}�B
bB
1�B

	B	�0B	�pB	�B	�B	�gB	�&B	��B	�-B	��B	v�B	g5B	J�B	9B	$�B	iB	)B	$B	B	�B	�B��B��B�B�dB�OB�>B�1B�%B�&B�*B�<B�ZB��B��B	�B	0B	"�B	R�B	kHB	��B	�B	�-B	�PB	�vB	��B	��B	��B	�B	�EB	�\B	ˉB	ΝB	ЧB	��B	�B	�QB	�|B	��B	��B	��B	��B	��B
�B
�B
�B
	�B
B
B
B
B
4B
BB
@B
BB
:B
;B
OB
TB
SB
RB
TB
aB
aB
_B
_B
aB
_B
YB
ZB
ZB
bB
ZB
RB
UB
RB
UB
UB
UB
\B
SB
SB
XB
[B
XB
aB
aB
xB
vB
qB
fB
RB
KB
aB
tB
lB
[B
LB
WB
oB
kB
eB
cB
_B
aB
eB
kB
qB
pB
uB
xB
xB
vB
pB
qB
pB
kB
iB
jB
eB
QB
DB
?B
@B
=B
LB
QB
RB
MB
JB
EB
LB
NB
FB
BB
DB
RB
VB
VB
WB
PB
IB
KB
CB
@B
=B
@B
8B
9B
;B
6B
3B
9B
9B
2B
:B
3B
:B
DB
@B
9B
9B
9B
1B
2B
-B
%B
B
B
B
�B
�B
�B
�B
�B
B
B
B
	�B
�B
�B
�B
�B
 �B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�vB	��B	�B	�yB	�wB	�xB	�xB	�xB	�xB	�rB	�vB	�nB	�qB	��B	�yB	�pB	�`B	�TB	�AB	�(B	�B	�B	��B	��B	��B	��B	�B	�B	�B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�!B	�(B	�%B	�0B	�/B	�.B	�5B	�4B	�3B	�<B	�8B	�CB	�@B	�BB	�@B	�?B	�AB	�GB	�FB	�FB	�DB	�DB	�LB	�MB	�NB	�SB	�RB	�TB	�SB	�VB	�TB	�`B	�`B	�_B	�_B	�cB	�dB	�dB	�lB	�lB	�tB	�sB	�xB	�tB	�wB	�xB	�xB	�wB	��B	�}B	�~B	��B	��B	��B	��B	��B	�~B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	�B
	�B
	�B
	�B

�B
B
B
AB
bB
"�B
$�B
)�B
.�B
4�B
>2B
BEB
FdB
GgB
L�B
S�B
W�B
\�B
_�B
f B
j;B
nUB
rj11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.54 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             201904051007582019040510075820190405100758  AO  ARCAADJP                                                                    20181121125951    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181121125951  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181121125951  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190405100758  IP                  G�O�G�O�G�O�                