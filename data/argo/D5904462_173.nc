CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-21T12:59:55Z creation      
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
_FillValue                    �,Argo profile    3.1 1.2 19500101000000  20181121125955  20190405100800  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  5287                            2C  D   APEX                            6529                            072314                          846 @��4�|1   @��4��I�@/�vȴ9X�c��G�{1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   A   B   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C�C�C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+fD+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  DtffDys3D�3D�@ D�s3D�� D���D�6fD�l�D�� D�  D�@ D��3D�� D��D�)�D�ffD�3D��D�P D�l�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�z�@�G�A��A(��AH��Ah��A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�B(�B
(�B(�B(�B"(�B*(�B2(�B:(�BB(�BJ(�BR(�BZ(�Bb(�Bj(�Br(�Bz(�B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�G�B�{B�{B�{B�{B�{B�{B�{B�{B�{C �=C�=C��C��C�=C
�=C�=C�=C�=C�=C�=C�=C�=C�=C�=C�=C �=C"�=C$�=C&�=C(�=C*�=C,�=C.�=C0�=C2�=C4�=C6�=C8�=C:�=C<�=C>�=C@�=CB�=CD�=CF�=CH�=CJ�=CL�=CN�=CP�=CR�=CT�=CV�=CX�=CZ�=C\�=C^�=C`�=Cb�=Cd�=Cf�=Ch�=Cj�=Cl�=Cn�=Cp�=Cr�=Ct�=Cv�=Cx�=Cz�=C|�=C~�=C�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�Q�C�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�ED "�D ��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D	"�D	��D
"�D
��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D "�D ��D!"�D!��D""�D"��D#"�D#��D$"�D$��D%"�D%��D&"�D&��D'"�D'��D("�D(��D)"�D)��D*"�D*��D+(�D+��D,"�D,��D-"�D-��D."�D.��D/"�D/��D0"�D0��D1"�D1��D2"�D2��D3"�D3��D4"�D4��D5"�D5��D6"�D6��D7"�D7��D8"�D8��D9"�D9��D:"�D:��D;"�D;��D<"�D<��D="�D=��D>"�D>��D?"�D?��D@"�D@��DA"�DA��DB"�DB��DC"�DC��DD"�DD��DE"�DE��DF"�DF��DG"�DG��DH"�DH��DI"�DI��DJ"�DJ��DK"�DK��DL"�DL��DM"�DM��DN"�DN��DO"�DO��DP"�DP��DQ"�DQ��DR"�DR��DS"�DS��DT"�DT��DU"�DU��DV"�DV��DW"�DW��DX"�DX��DY"�DY��DZ"�DZ��D["�D[��D\"�D\��D]"�D]��D^"�D^��D_"�D_��D`"�D`��Da"�Da��Db"�Db��Dc"�Dc��Dd"�Dd��De"�De��Df"�Df��Dg"�Dg��Dh"�Dh��Di"�Di��Dj"�Dj��Dk"�Dk��Dl"�Dl��Dm"�Dm��Dn"�Dn��Do"�Do��Dp"�Dp��Dq"�Dq��Dr"�Dr��Ds"�Ds��Dt"�Dt��Dy��D�${D�QHD��{D��HD�
�D�G�D�~D��HD�HD�QHD��{D��HD���D�:�D�w�D��{D�D�aHD�~D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�t�A�n�A�n�A�l�A�l�A�l�A�jA�jA�l�A�l�A�p�A�r�A�t�A�r�A�n�A�ffA�jA�`BA�7LA�I�A�t�Aʇ+A�ZA�XA�(�A��A�"�A�A�A�?}Aɟ�A�A���A��mA��A�M�A�Q�A�VA�ZA��;AʅA���Aɇ+A���A��A��yA�ȴAȍPA�&�A���A�-A�O�AȬAǡ�A�1'A�z�A���A��A��A�G�A��TA�ȴA��A�K�A���A��yA�VA�\)A�`BA�
=A�ffA��#A���A���A���A�p�A��TA��hA�A��A�%A�
=A�=qA�A���A��HA��7A���A��-A���A�C�A��`A� �A�p�A�=qA���A��A�  A��A�;dA�VA�=qA��hA�wA{C�Ay�Av5?As��An5?AeƨAb�A`Q�A^ĜA]�wA\��A[G�AYO�AT�ASK�AR�AP�RAK�TAI��AG�
AE��ACp�ABVA?��A;��A9�A8bA6bNA5O�A2�`A0A.(�A,A�A+O�A+%A*�A, �A,��A.{A.5?A-��A-�TA-ƨA-&�A+?}A)�A'K�A'�A&�A$��A$�9A$^5A#�#A#��A#�TA$5?A"ĜA"jA 1'A�HA{A�7AffA��AhsA&�AȴAI�At�Az�A`BAn�A+A{A��A�A�`A;dA^5AJA��A��AQ�A��Ap�A��A��A(�AƨA�A�AM�A
�/A	C�A�!A(�A��AE�A�7A�jA;dA�A ��A ^5A ZA  �@���@���@�hs@��/@�A�@��@�V@���@��@�ȴ@�@�%@�+@�7@�  @��@�=q@�/@띲@�
=@���@�n�@�=q@���@�9@蛦@���@�\@�5?@��@��@��@��@�+@��m@㕁@�;d@�\)@�K�@�C�@�;d@�+@�33@�A�@���@���@�@㝲@�;d@�=q@ᙚ@�V@�(�@�;d@�v�@�n�@ާ�@�;d@�"�@�ȴ@��@݁@�7L@�V@���@�b@ۥ�@�@�$�@���@��@��;@�t�@��@ְ!@ְ!@�ȴ@և+@���@�hs@���@�1'@���@���@�t�@�;d@�o@��H@҇+@��T@�&�@���@Ь@�z�@ϥ�@Ϯ@ϥ�@�|�@�C�@��@�O�@̼j@ˮ@��y@�V@�J@�@�hs@�X@�O�@�&�@��`@� �@�"�@Ƈ+@���@Ų-@ř�@�G�@�Ĝ@�1'@��m@�|�@�33@�
=@���@+@��-@�r�@�1@���@���@��@�o@���@�M�@�@�`B@�Z@���@�l�@�+@�v�@��@��#@�O�@���@�j@�  @���@�dZ@�o@���@�~�@�n�@�J@�/@���@���@�z�@��@��;@���@�ƨ@���@�\)@��@�~�@�~�@�^5@���@��h@�V@���@�r�@�Z@��w@�
=@���@���@���@���@���@�ff@��T@�O�@��`@���@�j@�(�@��
@�+@���@�ȴ@���@��!@���@��+@�@�hs@�O�@�G�@�&�@�%@��`@��@� �@��m@��w@��@�;d@��@�v�@�-@��-@�/@���@���@��@�|�@�C�@�"�@��y@���@��@��-@�O�@�%@���@��9@���@� �@���@��;@�33@��@���@�V@��@�@���@�O�@�7L@��/@�(�@�  @��m@��
@�ƨ@��F@�|�@�+@���@�ȴ@�E�@�{@���@��@���@�X@�p�@�O�@��@���@���@��@�  @���@�+@�@��H@��\@�{@��-@��h@��7@��7@�p�@�%@���@���@�1'@�
=@}@u�h@q�@hA�@_�P@V{@M��@E/@=�@6E�@0 �@*=q@$��@��@7L@9X@b11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�t�A�n�A�n�A�l�A�l�A�l�A�jA�jA�l�A�l�A�p�A�r�A�t�A�r�A�n�A�ffA�jA�`BA�7LA�I�A�t�Aʇ+A�ZA�XA�(�A��A�"�A�A�A�?}Aɟ�A�A���A��mA��A�M�A�Q�A�VA�ZA��;AʅA���Aɇ+A���A��A��yA�ȴAȍPA�&�A���A�-A�O�AȬAǡ�A�1'A�z�A���A��A��A�G�A��TA�ȴA��A�K�A���A��yA�VA�\)A�`BA�
=A�ffA��#A���A���A���A�p�A��TA��hA�A��A�%A�
=A�=qA�A���A��HA��7A���A��-A���A�C�A��`A� �A�p�A�=qA���A��A�  A��A�;dA�VA�=qA��hA�wA{C�Ay�Av5?As��An5?AeƨAb�A`Q�A^ĜA]�wA\��A[G�AYO�AT�ASK�AR�AP�RAK�TAI��AG�
AE��ACp�ABVA?��A;��A9�A8bA6bNA5O�A2�`A0A.(�A,A�A+O�A+%A*�A, �A,��A.{A.5?A-��A-�TA-ƨA-&�A+?}A)�A'K�A'�A&�A$��A$�9A$^5A#�#A#��A#�TA$5?A"ĜA"jA 1'A�HA{A�7AffA��AhsA&�AȴAI�At�Az�A`BAn�A+A{A��A�A�`A;dA^5AJA��A��AQ�A��Ap�A��A��A(�AƨA�A�AM�A
�/A	C�A�!A(�A��AE�A�7A�jA;dA�A ��A ^5A ZA  �@���@���@�hs@��/@�A�@��@�V@���@��@�ȴ@�@�%@�+@�7@�  @��@�=q@�/@띲@�
=@���@�n�@�=q@���@�9@蛦@���@�\@�5?@��@��@��@��@�+@��m@㕁@�;d@�\)@�K�@�C�@�;d@�+@�33@�A�@���@���@�@㝲@�;d@�=q@ᙚ@�V@�(�@�;d@�v�@�n�@ާ�@�;d@�"�@�ȴ@��@݁@�7L@�V@���@�b@ۥ�@�@�$�@���@��@��;@�t�@��@ְ!@ְ!@�ȴ@և+@���@�hs@���@�1'@���@���@�t�@�;d@�o@��H@҇+@��T@�&�@���@Ь@�z�@ϥ�@Ϯ@ϥ�@�|�@�C�@��@�O�@̼j@ˮ@��y@�V@�J@�@�hs@�X@�O�@�&�@��`@� �@�"�@Ƈ+@���@Ų-@ř�@�G�@�Ĝ@�1'@��m@�|�@�33@�
=@���@+@��-@�r�@�1@���@���@��@�o@���@�M�@�@�`B@�Z@���@�l�@�+@�v�@��@��#@�O�@���@�j@�  @���@�dZ@�o@���@�~�@�n�@�J@�/@���@���@�z�@��@��;@���@�ƨ@���@�\)@��@�~�@�~�@�^5@���@��h@�V@���@�r�@�Z@��w@�
=@���@���@���@���@���@�ff@��T@�O�@��`@���@�j@�(�@��
@�+@���@�ȴ@���@��!@���@��+@�@�hs@�O�@�G�@�&�@�%@��`@��@� �@��m@��w@��@�;d@��@�v�@�-@��-@�/@���@���@��@�|�@�C�@�"�@��y@���@��@��-@�O�@�%@���@��9@���@� �@���@��;@�33@��@���@�V@��@�@���@�O�@�7L@��/@�(�@�  @��m@��
@�ƨ@��F@�|�@�+@���@�ȴ@�E�@�{@���@��@���@�X@�p�@�O�@��@���@���@��@�  @���@�+@�@��H@��\@�{@��-@��h@��7@��7@�p�@�%@���@���@�1'@�
=@}@u�h@q�@hA�@_�P@V{@M��@E/@=�@6E�@0 �@*=q@$��@��@7L@9X@b11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	_;B	_;B	_;B	_;B	_;B	^5B	^5B	^5B	_;B	_;B	_;B	`BB	_;B	_;B	_;B	^5B	^5B	]/B	YB	T�B	r�B	{�B	~�B	_;B	XB	YB	ffB	{�B	�7B	�FB	ŢB	ɺB	�B	��B
JB
�B
\)B
�1B
��B
��B
�-B
�?B
��B
�'B
�!B
�9B
��B
�B�B33B>wB\)B�B��B�/B�`B�B  B�B�B'�B,B:^B>wBA�BF�BK�BT�BYB\)BZBW
BB�B,B"�B!�B�BB��B�B�/B��B�jB��B��B�+Bw�BhsBM�B/B'�B�BB
��B
��B
�\B
r�B
ffB
^5B
N�B
?}B
$�B	��B	�B	��B	��B	�DB	k�B	D�B	49B	.B	(�B	%�B	!�B	�B	�B	�B	�B	oB	DB��B��B�B�B�HB�)B��B��B�#B�mB�sB�mB�HB��B��B�5B�yB�B��B	#�B	B�B	aHB	k�B	l�B	o�B	s�B	~�B	|�B	z�B	�B	�=B	�7B	�JB	�uB	��B	��B	��B	�-B	�XB	�B	�9B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�uB	�7B	t�B	jB	cTB	e`B	o�B	~�B	�=B	�3B	�LB	�FB	�FB	�3B	��B	��B	��B	��B	��B	��B	��B	��B	�{B	�JB	�B	�B	}�B	v�B	q�B	p�B	q�B	k�B	dZB	_;B	^5B	_;B	_;B	]/B	[#B	[#B	ZB	ZB	\)B	^5B	ffB	jB	n�B	o�B	k�B	ffB	dZB	bNB	gmB	k�B	l�B	l�B	p�B	r�B	y�B	}�B	�B	�B	�=B	�bB	�bB	�hB	�bB	�hB	�bB	�hB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�-B	��B	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�)B	�/B	�/B	�/B	�/B	�)B	�#B	�B	�B	�#B	�#B	�B	�B	�#B	�)B	�5B	�;B	�BB	�HB	�TB	�ZB	�ZB	�`B	�ZB	�`B	�`B	�`B	�`B	�`B	�`B	�`B	�fB	�`B	�ZB	�ZB	�`B	�fB	�`B	�`B	�ZB	�TB	�ZB	�TB	�TB	�TB	�TB	�TB	�`B	�fB	�sB	�sB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
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
B
B
B
B
B
B
B
%B
%B
%B
%B
+B
+B
1B
1B
	7B
	7B

=B
	7B
	7B

=B
DB
DB
DB
DB
JB
PB
JB
JB
JB
JB
PB
VB
VB
VB
VB
VB
\B
VB
VB
VB
VB
\B
\B
\B
bB
\B
\B
\B
\B
\B
\B
\B
bB
bB
hB
hB
hB
hB
hB
oB
uB
{B
{B
{B
{B
{B
�B
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
$�B
,B
1'B
6FB
:^B
=qB
@�B
G�B
M�B
R�B
W
B
[#B
_;B
dZB
gmB
k�B
p�B
t�B
y�B
|�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B	_B	_B	_B	_B	_B	^B	^B	^B	_B	_B	_B	`B	_B	_B	_B	^B	^B	]B	X�B	T�B	r�B	{�B	~�B	_B	W�B	X�B	f;B	{�B	�
B	�B	�tB	ɏB	��B	��B
B
_B
[�B
�B
�[B
��B
�B
�B
��B
��B
��B
�B
ʖB
�B\B3B>LB[�B��B��B�B�6B�VB��BcB|B'�B+�B:-B>IBAXBFwBK�BT�BX�B[�BY�BV�BBaB+�B"�B!�BOB�B��B�PB��BвB�9B��B�OB��Bw�Bh@BM�B.�B'�BlB �B
дB
��B
�'B
r{B
f/B
^ B
N�B
?HB
$�B	��B	��B	�NB	��B	�B	kOB	DaB	3�B	-�B	(�B	%�B	!�B	yB	QB	WB	KB	5B		B��B��B�pB�IB�B��BѯBˉB��B�/B�5B�/B�
B��BЩB��B�=B�fB��B	#�B	BPB	a	B	kFB	lLB	o`B	sxB	~�B	|�B	z�B	��B	��B	��B	�B	�4B	�PB	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�}B	�sB	�bB	�YB	�TB	�4B	��B	t}B	j=B	cB	e B	o^B	~�B	��B	��B	�B	�B	�B	��B	��B	�xB	�kB	��B	��B	��B	�oB	�XB	�:B	�B	��B	��B	}�B	v�B	qhB	pbB	qhB	kBB	dB	^�B	]�B	^�B	^�B	\�B	Z�B	Z�B	Y�B	Y�B	[�B	]�B	f#B	j<B	nTB	o\B	kDB	f"B	dB	b
B	g+B	kEB	lGB	lGB	p`B	rmB	y�B	}�B	��B	��B	��B	� B	� B	�&B	� B	�$B	�B	�(B	�GB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�GB	�uB	�B	˃B	̈B	ϝB	ТB	ΖB	͏B	̆B	ˁB	˃B	͋B	УB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�"B	�B	�B	�B	�B	�"B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�-B	�.B	�/B	�5B	�5B	�5B	�;B	�AB	�?B	�@B	�FB	�LB	�MB	�SB	�TB	�SB	�UB	�TB	�UB	�cB	�eB	�fB	�dB	�eB	�eB	�hB	�jB	�jB	�jB	�kB	�qB	�vB	�zB	�}B	�yB	�wB	�pB	�jB	�pB	�xB	�wB	�|B	�zB	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	�B
�B
�B
	�B

�B

�B

�B

�B
B

B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
"B
#B
 B
 B
B
(B
0B
3B
4B
5B
4B
4B
;B
6B
5B
5B
5B
3B
:B
BB
FB
NB
MB
SG�O�B
[B
$�B
+�B
0�B
6 B
:B
=-B
@<B
GhB
M�B
R�B
V�B
Z�B
^�B
dB
g(B
k?B
p_B
tyB
y�B
|�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.54 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             201904051008002019040510080020190405100800  AO  ARCAADJP                                                                    20181121125955    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181121125955  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181121125955  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190405100800  IP                  G�O�G�O�G�O�                