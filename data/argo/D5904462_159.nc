CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-21T12:59:50Z creation      
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
resolution        :�o     �  q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �|   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �|   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �t   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �0   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �4   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �D   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �H   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �L   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20181121125950  20190405100757  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  5287                            2C  D   APEX                            6529                            072314                          846 @�ݾ2�;1   @�ݾ��@0Ƨ-�d]7KƧ�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   A   A   @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�ffB�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  C   C  C  C  C  C
  C  C�C  C�C  C�fC  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CE�fCH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp�Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
y�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DKfDK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dts3Dy� D�3D�C3D��fD���D���D�C3D�vfD�� D�  D�C3D�y�D��3D��3D�P Dډ�D๚D��3D�P D�p D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @\(�@�G�@�G�A��A(��AH��Ah��A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�B(�B
(�B(�B(�B"(�B*(�B2(�B:(�BB(�BJ(�BR(�BZ(�Bb(�Bj(�Br(�Bz(�B�{B�{B�{B�{B�{B�{B�{B�G�B�G�B�{B�{B�{B�{B�{B�{B�{B�{B�z�B�{B�{B�{B�{B�{B�{B�{B�{B�{B�G�B�{B�{B�{B�{C �=C�=C�=C�=C�=C
�=C�=C��C�=C��C�=Cp�C�=C�=C�=C�=C �=C"�=C$�=C&�=C(�=C*�=C,�=C.�=C0�=C2�=C4�=C6�=C8�=C:�=C<�=C>�=C@�=CB�=CD�=CFp�CH�=CJ�=CL�=CN�=CP�=CR�=CT�=CV�=CX�=CZ�=C\�=C^�=C`�=Cb�=Cd�=Cf�=Ch�=Cj�=Cl�=Cn�=Cp��Cr�=Ct�=Cv�=Cx�=Cz�=C|�=C~�=C�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�ED "�D ��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D	"�D	��D
"�D
�)D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D "�D ��D!"�D!��D""�D"��D#"�D#��D$"�D$��D%"�D%��D&"�D&��D'"�D'��D("�D(��D)"�D)��D*"�D*��D+"�D+��D,"�D,��D-"�D-��D."�D.��D/"�D/��D0"�D0��D1"�D1��D2"�D2��D3"�D3��D4"�D4��D5"�D5��D6"�D6��D7"�D7��D8"�D8��D9"�D9��D:"�D:��D;"�D;��D<"�D<��D="�D=��D>"�D>��D?"�D?��D@"�D@��DA"�DA��DB"�DB��DC"�DC��DD"�DD��DE"�DE��DF"�DF��DG"�DG��DH"�DH��DI"�DI��DJ"�DJ��DK(�DK��DL"�DL��DM"�DM��DN"�DN��DO"�DO��DP"�DP��DQ"�DQ��DR"�DR��DS"�DS��DT"�DT��DU"�DU��DV"�DV��DW"�DW��DX"�DX��DY"�DY��DZ"�DZ��D["�D[��D\"�D\��D]"�D]��D^"�D^��D_"�D_��D`"�D`��Da"�Da��Db"�Db��Dc"�Dc��Dd"�Dd��De"�De��Df"�Df��Dg"�Dg��Dh"�Dh��Di"�Di��Dj"�Dj��Dk"�Dk��Dl"�Dl��Dm"�Dm��Dn"�Dn��Do"�Do��Dp"�Dp��Dq"�Dq��Dr"�Dr��Ds"�Ds��Dt"�Dt��DyD�${D�T{D���D��D�D�T{D���D��HD�HD�T{D���D��{D�{D�aHDښ�D���D��{D�aHD�HD��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A�{A�{A��A�
=A�1A�1A�
=A�A���A��A��`A��`A��TA��TA��`A��HA��HA��#A��/A��/A��/A��;A��;A��;A��;A��HA��HA��HA��TA��`A��mA��yA��A��A��A��A���A���A���A���A���A���A���A��;AԲ-A�x�A�I�Aӝ�A��A���A���A�jAŮA��HA�
=AÏ\A�+A�ZA��A��uA�r�A�l�A�oA��A�A�+A�~�A��A�"�A���A��\A�ƨA��A�5?A�{A���A��A�O�A���A��;A���A��A���A�?}A��#A�XA��PA���A�K�A���A���A�{A�jA�O�A�ffA��A�A|ffAv�Aq�^AoXAl��Ai�TAd�HAc�mAb~�A_�A[?}AY�AVJAR(�AM�AJ��AC33AAS�AA�AAx�A=+A:v�A9�A81A6�+A6^5A6=qA5�hA4�\A4�A2$�A1��A1��A17LA0��A0$�A/�
A/��A.�A.VA-�#A-�7A-�wA-ƨA,�A,�\A+x�A*(�A(�/A(z�A'�A'��A'G�A&ȴA&�A%A%p�A%VA$�A$  A#��A#K�A"�uA!�A!��A!x�A!S�A ��A Q�A��A�A��A1'AAG�A�+A�AZA1A��AXA;dA"�A��AA�AdZA33A�`AffAVA��Ax�A;dA^5A{A�AAO�A�jAp�A�A�9AQ�A1'A��A\)A�A�RA(�A�A��A\)A
�RA
VA
^5A	�hAz�AG�AjA�DA  A&�A��A`BAM�A�#At�AO�A
=A ��A ��A I�@��@�ff@���@�1'@��
@���@�E�@��@�G�@��`@�  @��!@�J@��@���@��@��@�
=@��@�$�@�G�@�1@�5?@�O�@�%@�&�@�9X@�;d@��H@��@�7@�bN@��H@��#@�O�@��/@�@�@�bN@�I�@�A�@�1'@��
@�P@�o@��#@�A�@߾w@ߍP@ߍP@�dZ@�C�@��@�@�ff@�v�@�^5@�/@�  @�A�@�bN@܋D@܋D@��
@��@��y@��@ڸR@ڧ�@ٙ�@���@���@�Ĝ@أ�@�(�@�+@�-@ղ-@���@�z�@�I�@���@Ӆ@�\)@���@җ�@�5?@���@���@љ�@�O�@��`@�9X@�
=@�v�@��@�@���@̼j@̴9@̣�@̃@�Q�@�I�@��@��;@ˮ@�S�@�-@�V@�Z@�ƨ@��@Ɨ�@�-@�@ŉ7@�`B@�&�@ļj@�r�@�A�@�b@Ý�@��H@\@�v�@�n�@��@�hs@���@���@���@��u@��@�bN@���@��@���@��P@�\)@�-@�X@�/@��`@��@���@�t�@�K�@�o@�ȴ@�~�@���@��@��@�A�@�1@�  @�ƨ@���@�;d@��@�~�@�V@��#@�p�@�O�@�?}@�%@�bN@��;@���@��P@�l�@���@�v�@�5?@�@���@���@��@�7L@��9@��D@�z�@�b@��@�dZ@�"�@��@���@��h@�?}@���@���@��j@�j@�1'@� �@��m@�  @��
@�S�@�"�@���@�~�@�V@�{@���@�hs@�V@���@�bN@�Q�@�9X@�1'@��@�l�@�o@�ȴ@���@�n�@�=q@�@�O�@��@��/@���@�1@���@���@��@��@�|�@�+@���@�ff@�V@�M�@�M�@�-@�x�@���@��j@�bN@�1'@��;@�l�@�"�@���@�5?@�@��h@���@�hs@���@�j@� �@���@��@��+@��u@��R@��P@|9X@v@lZ@cC�@XĜ@N��@F��@=�@6�@.v�@';d@ ��@�H@/@x�@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A�{A�{A��A�
=A�1A�1A�
=A�A���A��A��`A��`A��TA��TA��`A��HA��HA��#A��/A��/A��/A��;A��;A��;A��;A��HA��HA��HA��TA��`A��mA��yA��A��A��A��A���A���A���A���A���A���A���A��;AԲ-A�x�A�I�Aӝ�A��A���A���A�jAŮA��HA�
=AÏ\A�+A�ZA��A��uA�r�A�l�A�oA��A�A�+A�~�A��A�"�A���A��\A�ƨA��A�5?A�{A���A��A�O�A���A��;A���A��A���A�?}A��#A�XA��PA���A�K�A���A���A�{A�jA�O�A�ffA��A�A|ffAv�Aq�^AoXAl��Ai�TAd�HAc�mAb~�A_�A[?}AY�AVJAR(�AM�AJ��AC33AAS�AA�AAx�A=+A:v�A9�A81A6�+A6^5A6=qA5�hA4�\A4�A2$�A1��A1��A17LA0��A0$�A/�
A/��A.�A.VA-�#A-�7A-�wA-ƨA,�A,�\A+x�A*(�A(�/A(z�A'�A'��A'G�A&ȴA&�A%A%p�A%VA$�A$  A#��A#K�A"�uA!�A!��A!x�A!S�A ��A Q�A��A�A��A1'AAG�A�+A�AZA1A��AXA;dA"�A��AA�AdZA33A�`AffAVA��Ax�A;dA^5A{A�AAO�A�jAp�A�A�9AQ�A1'A��A\)A�A�RA(�A�A��A\)A
�RA
VA
^5A	�hAz�AG�AjA�DA  A&�A��A`BAM�A�#At�AO�A
=A ��A ��A I�@��@�ff@���@�1'@��
@���@�E�@��@�G�@��`@�  @��!@�J@��@���@��@��@�
=@��@�$�@�G�@�1@�5?@�O�@�%@�&�@�9X@�;d@��H@��@�7@�bN@��H@��#@�O�@��/@�@�@�bN@�I�@�A�@�1'@��
@�P@�o@��#@�A�@߾w@ߍP@ߍP@�dZ@�C�@��@�@�ff@�v�@�^5@�/@�  @�A�@�bN@܋D@܋D@��
@��@��y@��@ڸR@ڧ�@ٙ�@���@���@�Ĝ@أ�@�(�@�+@�-@ղ-@���@�z�@�I�@���@Ӆ@�\)@���@җ�@�5?@���@���@љ�@�O�@��`@�9X@�
=@�v�@��@�@���@̼j@̴9@̣�@̃@�Q�@�I�@��@��;@ˮ@�S�@�-@�V@�Z@�ƨ@��@Ɨ�@�-@�@ŉ7@�`B@�&�@ļj@�r�@�A�@�b@Ý�@��H@\@�v�@�n�@��@�hs@���@���@���@��u@��@�bN@���@��@���@��P@�\)@�-@�X@�/@��`@��@���@�t�@�K�@�o@�ȴ@�~�@���@��@��@�A�@�1@�  @�ƨ@���@�;d@��@�~�@�V@��#@�p�@�O�@�?}@�%@�bN@��;@���@��P@�l�@���@�v�@�5?@�@���@���@��@�7L@��9@��D@�z�@�b@��@�dZ@�"�@��@���@��h@�?}@���@���@��j@�j@�1'@� �@��m@�  @��
@�S�@�"�@���@�~�@�V@�{@���@�hs@�V@���@�bN@�Q�@�9X@�1'@��@�l�@�o@�ȴ@���@�n�@�=q@�@�O�@��@��/@���@�1@���@���@��@��@�|�@�+@���@�ff@�V@�M�@�M�@�-@�x�@���@��j@�bN@�1'@��;@�l�@�"�@���@�5?@�@��h@���@�hs@���@�j@� �@���@��@��+@��u@��R@��P@|9X@v@lZ@cC�@XĜ@N��@F��@=�@6�@.v�@';d@ ��@�H@/@x�@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	k�B	k�B	k�B	k�B	k�B	k�B	k�B	k�B	k�B	k�B	k�B	m�B	l�B	m�B	m�B	m�B	m�B	n�B	o�B	o�B	o�B	o�B	o�B	p�B	q�B	r�B	r�B	s�B	s�B	u�B	w�B	w�B	w�B	y�B	y�B	}�B	�B	�B	�DB	�VB	�oB	��B	��B	��B	��B	��B	�B	�-B	ÖB
{B
�BhBN�B{�B�1B��B��B�'B�wBÖBĜB�#B�B��B�LB��B��B��B��BǮB�qB�}B��B��Bo�BffBl�B8RB"�B\)Bp�BS�B@�B$�B\B
��B
�HB
��B
�B
�=B
XB
1B	�5B	ÖB	��B	�B	y�B	q�B	ffB	hsB	t�B	l�B	e`B	[#B	L�B	F�B	@�B	;dB	1'B	(�B	#�B	�B��B�ZB�B�TB�B	oB	-B	;dB	>wB	B�B	[#B	]/B	^5B	e`B	o�B	r�B	�B	�+B	�1B	�DB	�{B	��B	��B	��B	��B	�B	�B	�XB	��B	�B	�5B	�/B	�;B	�fB	�ZB	�ZB	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B
  B
B
1B
JB
PB
PB
PB
VB
bB
bB
bB
uB
�B
�B
�B
�B
�B
hB
bB
oB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
!�B
 �B
�B
�B
VB
B	��B
  B
B
B
B
B
B
B
B
  B	��B	��B	��B
  B	��B	�B	�`B	�HB	�fB	�TB	�;B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ɺB	ƨB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ǮB	ƨB	ŢB	ǮB	ȴB	ǮB	ƨB	ŢB	ĜB	ĜB	ÖB	ŢB	ǮB	ǮB	ǮB	ǮB	ȴB	ȴB	ǮB	ǮB	ȴB	ǮB	ǮB	ǮB	ǮB	ǮB	ǮB	ǮB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	�B	�#B	�)B	�)B	�5B	�5B	�5B	�5B	�;B	�5B	�HB	�NB	�NB	�HB	�HB	�HB	�HB	�HB	�NB	�TB	�NB	�NB	�NB	�TB	�TB	�NB	�TB	�TB	�ZB	�`B	�`B	�`B	�`B	�`B	�`B	�`B	�`B	�`B	�fB	�fB	�fB	�fB	�fB	�mB	�fB	�fB	�mB	�fB	�fB	�TB	�;B	�/B	�5B	�;B	�HB	�NB	�TB	�TB	�ZB	�ZB	�`B	�`B	�mB	�sB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
%B
1B
1B
1B
1B
	7B
	7B
	7B

=B

=B

=B

=B
DB
DB
DB
DB
DB
DB
DB
JB
JB
JB
JB
JB
JB
PB
VB
VB
\B
VB
\B
bB
bB
bB
bB
VB
VB
\B
\B
VB
\B
bB
hB
{B
�B
�B
"�B
$�B
,B
8RB
;dB
A�B
D�B
G�B
M�B
S�B
YB
`BB
ffB
jB
o�B
v�B
z�B
~�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	k[B	kYB	k\B	kXB	kXB	kYB	k[B	kYB	k[B	k[B	kZB	mfB	lcB	mfB	meB	mfB	mhB	nnB	otB	otB	owB	owB	owB	pwB	q�B	r�B	r�B	s�B	s�B	u�B	w�B	w�B	w�B	y�B	y�B	}�B	��B	��B	�B	�,B	�EB	�VB	�hB	��B	��B	��B	��B	�B	�jB
MB
�vB<BN�B{�B�B��B��B��B�HB�hB�nB��B�PBʑB�B˙B��B��B��B�~B�@B�MB��B�XBoqBf6BlUB8B"�B[�BprBS�B@MB$�B*B
��B
�B
�TB
��B
�	B
W�B
�B	� B	�_B	�^B	��B	y�B	qsB	f,B	h;B	t�B	lSB	e(B	Z�B	L�B	FoB	@LB	;)B	0�B	(�B	#�B	mB��B�B��B�B�{B	4B	,�B	;(B	>9B	BTB	Z�B	\�B	]�B	e"B	oaB	rqB	��B	��B	��B	�B	�>B	�hB	��B	��B	��B	��B	��B	�B	̎B	��B	��B	��B	��B	�'B	�B	�B	�AB	�LB	�QB	�eB	�|B	��B	��B	��B	��B	��B	��B
�B
�B

B
B
B
B
B
#B
!B
#B
5B
DB
HB
HB
GB
BB
'B
"B
0B
;B
@B
JB
HB
[B
fB
gB
eB
pB
rB
zB
xB
wB
eB
sB
!�B
 �B
qB
QB
B
�B	��B	��B
�B
�B
�B
�B
�B
 �B
 �B	��B	��B	��B	��B	��B	��B	�WB	�B	�B	�%B	�B	��B	��B	ҰB	͑B	̊B	ϝB	ѫB	ТB	ѨB	ѫB	ϞB	�}B	�xB	�dB	�oB	�yB	�wB	�}B	˄B	˄B	�}B	�~B	˂B	�}B	�~B	�~B	�~B	�~B	˅B	�~B	̉B	˅B	�}B	�kB	�dB	�aB	�kB	�sB	�jB	�eB	�`B	�ZB	�XB	�SB	�]B	�lB	�jB	�iB	�jB	�qB	�pB	�lB	�kB	�oB	�jB	�kB	�iB	�jB	�lB	�kB	�jB	�oB	�pB	�vB	ˀB	͏B	ѧB	ӵB	үB	ҲB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�
B	�B	�B	�	B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�B	� B	� B	�"B	�)B	�B	�$B	�)B	�!B	�#B	�B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�)B	�-B	�/B	�5B	�2B	�2B	�8B	�@B	�@B	�RB	�ZB	�aB	�bB	�ZB	�\B	�XB	�YB	�ZB	�SB	�LB	�7B	�AB	�DB	�GB	�NB	�NB	�JB	�LB	�SB	�TB	�^B	�eB	�dB	�eB	�dB	�fB	�cB	�kB	�oB	�pB	�zB	�wB	�rB	�B	��B	��B	�B	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	�B
	�B
	�B
	�B

�B

�B

�B
 B
 B
 B

�B
B
B
B
B
B
B
	B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
"B
5B
HB
fB
"�B
$�B
+�B
8B
;B
ACB
DUB
GhB
M�B
S�B
X�B
_�B
fB
j9B
oWB
v�B
z�B
~�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.54 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             201904051007572019040510075720190405100757  AO  ARCAADJP                                                                    20181121125950    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181121125950  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181121125950  QCF$                G�O�G�O�G�O�4000            UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190405100757  IP                  G�O�G�O�G�O�                