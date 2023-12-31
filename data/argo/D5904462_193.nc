CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2017-05-27T17:02:58Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         (Matthew Alkire, University of Washington      @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
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
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20170527170258  20190405100803  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  5287                            2C  D   APEX                            6529                            072314                          846 @�
�F2"1   @�
��d��@-���+�d�S���1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�33B�  B�  B�  B�  B�33B�  B�ffB�ffB�  B�  B�  B�  B�  B�  B�  B�33B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B���C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL�CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C}�fC�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DBfDB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DOy�DP  DP� DQ  DQ� DQ��DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDys3D� D�I�D�p D��fD�� D�,�D���D�� D�3D�6fD���Dǳ3D��D�L�Dډ�D�3D�3D�<�D�y�D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�z�@�G�A��A(��AH��Ah��A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�B(�B
(�B(�B(�B"(�B*(�B2(�B:(�BB(�BJ(�BR(�BZ(�Bb(�Bj(�Br(�Bz(�B�{B�G�B�{B�{B�{B�{B�G�B�{B�z�B�z�B�{B�{B�{B�{B�{B�{B�{B�G�B��HB�{B�{B�{B�{B�{B�{B�{B�{B�{B�G�B�{B�{B��HC �=C�=C�=C�=C�=C
�=C�=C�=C�=C�=C�=C�=C�=C�=C�=C�=C �=C"�=C$�=C&�=C(�=C*�=C,�=C.�=C0�=C2�=C4�=C6�=C8�=C:�=C<�=C>�=C@�=CB�=CD�=CF�=CH�=CJ�=CL��CN�=CP�=CR�=CT�=CV�=CX�=CZ�=C\�=C^�=C`�=Cb�=Cd�=Cf�=Ch�=Cj�=Cl�=Cn�=Cp�=Cr�=Ct�=Cv�=Cx�=Cz�=C|�=C~p�C�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�Q�C�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�ED "�D ��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D	"�D	��D
"�D
��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D "�D ��D!"�D!��D""�D"��D#"�D#��D$"�D$��D%"�D%��D&"�D&��D'"�D'��D("�D(��D)"�D)��D*"�D*��D+"�D+��D,"�D,��D-"�D-��D."�D.��D/"�D/��D0"�D0��D1"�D1��D2"�D2��D3"�D3��D4"�D4��D5"�D5��D6"�D6��D7"�D7��D8"�D8��D9"�D9��D:"�D:��D;"�D;��D<"�D<��D="�D=��D>"�D>��D?"�D?��D@"�D@��DA"�DA��DB(�DB��DC"�DC��DD"�DD��DE"�DE��DF"�DF��DG"�DG��DH"�DH��DI"�DI��DJ"�DJ��DK"�DK��DL"�DL��DM"�DM��DN"�DN��DO"�DO�)DP"�DP��DQ"�DQ��DR)DR��DS"�DS��DT"�DT��DU"�DU��DV"�DV��DW"�DW��DX"�DX��DY"�DY��DZ"�DZ��D["�D[��D\"�D\��D]"�D]��D^"�D^��D_"�D_��D`"�D`��Da"�Da��Db"�Db��Dc"�Dc��Dd"�Dd��De"�De��Df"�Df��Dg"�Dg��Dh"�Dh��Di"�Di��Dj"�Dj��Dk"�Dk��Dl"�Dl��Dm"�Dm��Dn"�Dn��Do"�Do��Dp"�Dp��Dq"�Dq��Dr"�Dr��Ds"�Ds��Dt"�Dt��Dt��Dy��D�!HD�Z�D��HD���D�HD�>D��D��HD�{D�G�D���D��{D�*�D�^Dښ�D��{D�${D�ND��D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A�VA��`AуA�jA�S�A�=qA��A�1A���A��A���Aв-AХ�AП�AН�AН�AЛ�AЛ�AЙ�AЉ7AЃA�z�A�t�A�p�A�l�A�hsA�dZA�VA�E�A�C�A�A�A�5?A�$�A��A��A�VA���A�C�A��#A�O�A�?}A�1'A�ffAĥ�A�VA���A�A�v�A��FA�|�A���A��!A��^A��A�z�A�hsA�
=A��;A�5?A���A��A�-A��;A�ȴA�ƨA��A�VA���A��+A�I�A���A�7LA��A��`A�jA���A���A�7LA��wA�ȴA�x�A�/A���A|E�AxI�Ao/Ak/Af�yAa�7A^ �A]��A]%A\JAY�AX$�AW��AT��ARȴARA�AQ�#AP�jAOK�AL��AKAI�AHffAEAC�
AB�AA/A?�A>A�A9\)A7"�A4ĜA3�A3K�A2^5A1ƨA1
=A0�A/��A/��A/�A-�A,=qA+��A+%A*A�A)��A*JA)��A)
=A(�A(Q�A'S�A%�#A$�A$��A$�uA$Q�A#�A!"�A��A��A�TA I�A  �A�mA�#A;dA��AdZA%A��A�DAjA{A�A�9AVA�-AO�AoA��A�9AdZAn�A�A�yA�!A�jA�AȴA��A��A��A&�A�A�uAVA  A�TA�FA"�A9XA��AO�A��A�A��A/A�`A�+AE�A�;A�AVA�!AbNA�#A�A`BA�RA-A�A�TAx�A��A��A^5A�A�mA�A7LA
�\A
(�A	��A	XA	7LA�`A�9A�A��AA��A�9AZA(�A�A�^A\)A��A��AbNA1AG�AȴAv�A=qAZAbNA-AA&�A ȴA �A Z@���@���@��@��@�bN@���@���@��!@���@��F@���@���@��-@�%@�r�@�F@�V@�@��@�9X@�F@�+@��@�@�ƨ@�@ꟾ@��@�O�@���@�u@�1'@�S�@���@�J@�p�@�z�@���@㝲@�S�@�C�@���@��@���@�9X@�dZ@�5?@ܴ9@�~�@أ�@��@��y@�n�@��T@�A�@�33@ҧ�@��@Ѳ-@���@���@ύP@��@���@�I�@��@�1'@̣�@�j@�  @��@�1@��@�ƨ@�l�@�@Ǖ�@�ȴ@�S�@��y@�$�@��#@�p�@��@ģ�@���@Å@�;d@�^5@��^@�O�@�X@�r�@�I�@��9@���@� �@��@�
=@��@���@�b@��m@��
@���@�1@���@��@���@�v�@�v�@�~�@��R@��H@��!@���@��9@�  @���@�t�@�dZ@�K�@�33@�"�@�;d@�K�@���@�ff@�V@���@�`B@���@�^5@��@�?}@���@���@��j@�Ĝ@���@�I�@�(�@�  @��F@�t�@�o@�ȴ@���@�~�@�$�@���@��7@��@���@��D@��m@�\)@��@���@�ff@�@�`B@�%@���@�z�@�I�@��@��F@�C�@��@�{@���@��@��@�z�@�bN@�Q�@� �@��m@��
@�S�@�
=@��R@�@���@��@�O�@��j@��@��@��u@�I�@��@��m@���@��H@���@�-@�E�@���@��@�&�@���@�z�@�bN@�1@�l�@���@���@�{@�X@��/@�bN@�  @�|�@�l�@�K�@�
=@���@���@���@�n�@�@��@�x�@�O�@��@�%@���@�Z@��@���@�K�@�
=@���@�ff@�@�@�?}@���@���@�p�@���@�\)@|��@t9X@kt�@`Q�@U`B@Mp�@F5?@?��@8Q�@/l�@)��@"�!@��@v�@��@�T@
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A��A�VA��`AуA�jA�S�A�=qA��A�1A���A��A���Aв-AХ�AП�AН�AН�AЛ�AЛ�AЙ�AЉ7AЃA�z�A�t�A�p�A�l�A�hsA�dZA�VA�E�A�C�A�A�A�5?A�$�A��A��A�VA���A�C�A��#A�O�A�?}A�1'A�ffAĥ�A�VA���A�A�v�A��FA�|�A���A��!A��^A��A�z�A�hsA�
=A��;A�5?A���A��A�-A��;A�ȴA�ƨA��A�VA���A��+A�I�A���A�7LA��A��`A�jA���A���A�7LA��wA�ȴA�x�A�/A���A|E�AxI�Ao/Ak/Af�yAa�7A^ �A]��A]%A\JAY�AX$�AW��AT��ARȴARA�AQ�#AP�jAOK�AL��AKAI�AHffAEAC�
AB�AA/A?�A>A�A9\)A7"�A4ĜA3�A3K�A2^5A1ƨA1
=A0�A/��A/��A/�A-�A,=qA+��A+%A*A�A)��A*JA)��A)
=A(�A(Q�A'S�A%�#A$�A$��A$�uA$Q�A#�A!"�A��A��A�TA I�A  �A�mA�#A;dA��AdZA%A��A�DAjA{A�A�9AVA�-AO�AoA��A�9AdZAn�A�A�yA�!A�jA�AȴA��A��A��A&�A�A�uAVA  A�TA�FA"�A9XA��AO�A��A�A��A/A�`A�+AE�A�;A�AVA�!AbNA�#A�A`BA�RA-A�A�TAx�A��A��A^5A�A�mA�A7LA
�\A
(�A	��A	XA	7LA�`A�9A�A��AA��A�9AZA(�A�A�^A\)A��A��AbNA1AG�AȴAv�A=qAZAbNA-AA&�A ȴA �A Z@���@���@��@��@�bN@���@���@��!@���@��F@���@���@��-@�%@�r�@�F@�V@�@��@�9X@�F@�+@��@�@�ƨ@�@ꟾ@��@�O�@���@�u@�1'@�S�@���@�J@�p�@�z�@���@㝲@�S�@�C�@���@��@���@�9X@�dZ@�5?@ܴ9@�~�@أ�@��@��y@�n�@��T@�A�@�33@ҧ�@��@Ѳ-@���@���@ύP@��@���@�I�@��@�1'@̣�@�j@�  @��@�1@��@�ƨ@�l�@�@Ǖ�@�ȴ@�S�@��y@�$�@��#@�p�@��@ģ�@���@Å@�;d@�^5@��^@�O�@�X@�r�@�I�@��9@���@� �@��@�
=@��@���@�b@��m@��
@���@�1@���@��@���@�v�@�v�@�~�@��R@��H@��!@���@��9@�  @���@�t�@�dZ@�K�@�33@�"�@�;d@�K�@���@�ff@�V@���@�`B@���@�^5@��@�?}@���@���@��j@�Ĝ@���@�I�@�(�@�  @��F@�t�@�o@�ȴ@���@�~�@�$�@���@��7@��@���@��D@��m@�\)@��@���@�ff@�@�`B@�%@���@�z�@�I�@��@��F@�C�@��@�{@���@��@��@�z�@�bN@�Q�@� �@��m@��
@�S�@�
=@��R@�@���@��@�O�@��j@��@��@��u@�I�@��@��m@���@��H@���@�-@�E�@���@��@�&�@���@�z�@�bN@�1@�l�@���@���@�{@�X@��/@�bN@�  @�|�@�l�@�K�@�
=@���@���@���@�n�@�@��@�x�@�O�@��@�%@���@�Z@��@���@�K�@�
=@���@�ff@�@�@�?}G�O�@���@�p�@���@�\)@|��@t9X@kt�@`Q�@U`B@Mp�@F5?@?��@8Q�@/l�@)��@"�!@��@v�@��@�T@
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	�B	�B	!�B	/B	0!B	0!B	0!B	/B	.B	-B	,B	(�B	&�B	%�B	%�B	%�B	%�B	%�B	%�B	%�B	$�B	$�B	$�B	$�B	$�B	$�B	$�B	%�B	(�B	,B	,B	-B	.B	/B	.B	-B	-B	2-B	q�B	�^B
+B
9XB
p�B
�%B
�B
�ZB�B6FBk�BgmBv�B�B�bB�hB�uB�{B�oB��B�!B�B�'B�LBɺB�BB��B��B�BD�B�B
��B
�NB
�}B
��B
o�B
M�B
8RB
-B
"�B
"�B
 �B
�B
  B	�BB	��B	jB	N�B	-B	�B	�B	hB	+B	B	B��B��B	+B	
=B	B	B	�B	�B	�B	\B	B��B��B�B�ZB�HB�HB�;B�#B�
BB�?B�B�9B�LB�#B�B��B		7B	�B	�B	'�B	<jB	H�B	D�B	B�B	=qB	N�B	ZB	ffB	hsB	k�B	m�B	l�B	hsB	q�B	u�B	z�B	z�B	r�B	jB	ffB	hsB	x�B	�DB	�\B	�uB	��B	�{B	�DB	�B	�%B	�\B	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�LB	��B	��B	��B	�B	�;B	�mB	�yB	�B	�B	��B	��B

=B

=B
JB
bB
bB
bB
hB
oB
uB
{B
oB
oB
uB
{B
�B
�B
�B
{B
�B
�B
{B
oB
oB
oB
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
hB
PB
DB
	7B
1B
DB
VB
JB
DB

=B

=B
	7B
1B
%B
B
B
B
  B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�mB	�ZB	�HB	�BB	�5B	�)B	�)B	�5B	�BB	�;B	�/B	�#B	�5B	�BB	�5B	�)B	�#B	�#B	�;B	�ZB	�fB	�mB	�B	�B	�B	�B	�B	�B	�mB	�yB	�B	�B	�B	�B	�B	�B	�yB	�sB	�mB	�mB	�fB	�mB	�fB	�fB	�fB	�mB	�B	�B	�B	�B	�yB	�fB	�ZB	�ZB	�fB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
B
%B
%B
+B
+B
+B
+B
1B
	7B
	7B
	7B
	7B
	7B
	7B

=B

=B
DB
DB
JB
PB
PB
PB
PB
PB
VB
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
\B
\B
\B
bB
bB
bB
bB
\B
VB
oB
oB
oB
hB
hB
oB
hB
hB
bB
bB
\B
\B
\B
bB
bB
bB
hB
oB
oB
uB
{B
{B
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
"�B
�B
#�B
)�B
-B
49B
8RB
A�B
F�B
K�B
N�B
Q�B
W
B
\)B
bNB
ffB
k�B
q�B
v�B
{�B
� B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B	�B	�B	!�B	.�B	/�B	/�B	/�B	.�B	-�B	,�B	+�B	(�B	&�B	%�B	%�B	%�B	%�B	%�B	%�B	%�B	$�B	$�B	$�B	$�B	$�B	$�B	$�B	%�B	(�B	+�B	+�B	,�B	-�B	.�B	-�B	,�B	,�B	2B	q}B	�4B
�B
9*B
ptB
��B
��B
�-BvB6BkVBg@Bv�B��B�5B�9B�HB�IB�?B�WB��B��B��B�BɇB�BʐB��B��BDkBaB
��B
�B
�JB
��B
oiB
M�B
8!B
,�B
"�B
"�B
 �B
QB	��B	�B	��B	jHB	N�B	,�B	tB	mB	.B	�B	�B	�B��B��B	�B	
B	�B	�B	XB	kB	GB	!B	�B��B�B�\B�!B�B�B�B��B��B�QB�B��B��B�B��B�[B��B	�B	AB	|B	'�B	<+B	HuB	D^B	BRB	=2B	N�B	Y�B	f'B	h4B	kGB	mQB	lKB	h5B	qkB	u�B	z�B	z�B	roB	j@B	f'B	h4B	x�B	�B	�B	�6B	�AB	�<B	�B	��B	��B	�B	�7B	�MB	�gB	�GB	�TB	�`B	�wB	��B	��B	��B	��B	�B	̎B	УB	ԿB	��B	��B	�0B	�9B	�LB	�kB	�|B	��B
	�B
	�B
B
"B
$B
#B
'B
2B
4B
=B
0B
.B
2B
<B
?B
@B
@B
;B
?B
BB
;B
0B
/B
0B
AB
?B
9B
EB
LB
GB
IB
DB
DB
EB
FB
FB
EB
FB
RB
YB
PB
SB
KB
PB
JB
EB
?B
QB
[B
SB
IB
=B
=B
HB
DB
?B
;B
'B
B
 B
�B
�B
B
B
B
B
	�B
	�B
�B
�B
�B
�B
�B
�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�{B	�tB	�sB	�sB	�rB	�mB	�nB	�mB	�dB	�hB	�`B	�YB	�TB	�TB	�VB	�SB	�UB	�TB	�aB	�_B	�`B	�aB	�cB	�TB	�@B	�6B	�)B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�)B	�WB	�`B	�]B	�[B	�QB	�@B	�)B	�4B	�KB	�MB	�CB	�?B	�BB	�9B	�2B	�-B	�*B	�*B	� B	�*B	�!B	�!B	� B	�(B	�NB	�LB	�KB	�IB	�6B	� B	�B	�B	�B	�6B	�;B	�NB	�FB	�HB	�SB	�XB	�[B	�_B	�vB	��B	�B	�jB	�^B	�SB	�LB	�LB	�UB	�WB	�VB	�^B	�lB	�lB	�vB	�rB	�pB	�vB	�xB	��B	��B	��B	��B	��B	��B	��B	��B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
B
	B
	B

B

B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
+B
*B
'B
"B
 B
)B
#B
"B
B
B
B
B
B
B
B
B
#B
*B
*B
/B
5B
5B
4B
5B
7B
3B
6B
9B
:B
<B
AB
BB
GB
?B
FB
AB
GB
KB
LB
MB
TG�O�B
jB
#�B
)�B
,�B
3�B
8B
ADB
FaB
K�B
N�B
Q�B
V�B
[�B
b	B
f B
k>B
qfB
v�B
{�B
�B
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.54 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             201904051008032019040510080320190405100803  AO  ARCAADJP                                                                    20170527170258    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20170527170258  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20170527170258  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190405100803  IP                  G�O�G�O�G�O�                