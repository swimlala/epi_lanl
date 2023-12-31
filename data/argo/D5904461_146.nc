CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-21T12:58:30Z creation      
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
_FillValue                    �,Argo profile    3.1 1.2 19500101000000  20181121125830  20190408133248  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  5286                            2C  D   APEX                            6531                            072314                          846 @�у���s1   @�фq�@4�?|�h�b�^5?|�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C�fC  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�3Dy��D�fD�6fD�|�D���D�3D�S3D�p D��fD�  D�9�D�� D��fD�  D�C3D�y�D���D���D�9�D�c311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��G@�z�A=qA"=qAB=qAb=qA��A��A��A��A��A��A��A��B �\B�\B�\B�\B �\B(�\B0�\B8�\B@�\BH�\BP�\BX�\B`�\Bh�\Bp�\Bx�\B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�z�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�C #�C#�C#�C#�C#�C
#�C#�C#�C#�C#�C
=C#�C#�C#�C#�C#�C #�C"#�C$#�C&#�C(#�C*#�C,#�C.#�C0#�C2#�C4#�C6#�C8#�C:#�C<#�C>#�C@#�CB#�CD#�CF#�CH#�CJ#�CL#�CN#�CP#�CR#�CT#�CV#�CX#�CZ#�C\#�C^#�C`#�Cb#�Cd#�Cf#�Ch#�Cj#�Cl#�Cn#�Cp#�Cr#�Ct#�Cv#�Cx#�Cz#�C|#�C~#�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Dt�)Dy��D��D�:�D��HD��HD��D�W�D�t{D���D�{D�>D��{D���D�{D�G�D�~D��HD�HD�>D�g�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�G�A�?}A�+A�oA�A���Aϧ�AυA�p�A�O�A�(�A� �A�bA�
=A�A���A��A��yA��HA��;A��#A��
A���A���A�ƨA���AξwAθRAβ-Aβ-Aβ-Aΰ!AΧ�AΝ�AΛ�AΙ�AΗ�A΁A��;A̴9A��A���A�ȴA��A©�A���A�VA���A��HA�I�A�(�A�%A�bNA�jA��wA�ZA�dZA�n�A��A��9A���A��A���A�  A�v�A���A��A�ĜA�ĜA���A�7LA���A��wA�?}A�=qA��A��A�O�A��FA��A���A�ĜA�9XA��A��A�A��7A���A�S�A�A��\A�VA��TA�9XA�ȴA�hsA�z�A��9A��DA��yA���A��hA�VA�A�jA�A���A��A�ZA�ffA���A�ZA��#Az�RAt��Aql�An �Ak��Ah�DAe��Ac�mAc\)Ab�DAaVA_�A^5?A\��AZz�AY+AXbNAWVAT��AR�AP�!ANȴAL��AKC�AH�uAF��AE�7AD(�ABz�A@^5A>z�A<�A;`BA:�uA9?}A8(�A7"�A5�#A4��A3VA1
=A/ƨA/XA.-A-hsA-&�A,VA+XA*jA(�A'S�A%?}A$  A#�A"�HA#oA#l�A#K�A"�HA"ffA!dZA �HAO�Ap�AA�A��A�`A-A��AoA^5A^5A�/A��AA
=Al�A�AC�AJA��A�HAJA+A	��A�+At�A�!AƨAXAA?}A ��@�;d@�x�@�+@���@��-@�b@�ȴ@�K�@�v�@�@�5?@��/@�b@�C�@��@���@�$�@�%@�dZ@��y@�G�@�7L@�r�@�;d@�@ۥ�@��@��/@�I�@ם�@�G�@���@ա�@���@ՙ�@֧�@ա�@�I�@٩�@��#@�O�@ؓu@�1'@ם�@֧�@�G�@�l�@ҸR@�J@�V@�K�@Ο�@�O�@�33@��@��;@��@�C�@��y@�Ĝ@��
@�\)@���@¸R@�M�@�$�@���@��
@�+@���@�E�@��@�@��@��T@��#@�hs@��D@�|�@�-@�&�@���@�z�@�Q�@�1'@�dZ@���@�ff@�-@��@��@�dZ@�
=@���@��@�
=@���@���@�Ĝ@�p�@�p�@���@��!@��@�b@��@�/@�/@�&�@�X@�x�@��@���@�p�@��@��@�z�@� �@��P@��@��@��@��P@�l�@��@��H@��H@�ȴ@�-@���@��@��^@�%@�bN@�  @�l�@�@��!@�~�@�-@��@��T@���@���@�X@�G�@�?}@�&�@�%@�V@���@��j@�A�@�1@��@���@���@�t�@�;d@�ȴ@���@�^5@��@���@���@��h@�O�@�%@�V@���@���@���@�A�@�b@���@��w@���@�t�@�
=@�^5@���@�@���@�p�@��@�r�@�1'@�ƨ@��@�M�@�5?@��@��#@���@��@��@���@�Ĝ@��@� �@��@�t�@��y@�ȴ@��R@���@�E�@��@�`B@�7L@��@��@�Ĝ@��j@��9@���@�z�@�j@�1'@���@��@��@�C�@��@�n�@���@���@��7@�`B@�O�@�7L@���@�9X@���@��@�l�@�\)@�S�@�C�@�33@�+@��@�ȴ@��+@�5?@�$�@��@��@��@�/@���@�bN@�A�@�(�@�(�@��@�  @�"�@���@�x�@�O�@��@���@���@�;d@��@���@��7@���@���@��
@�
=@��y@�33@�1@���@��+@�{@��7@��7@�O�@�O�@�x�@�O�@���@}��@r�!@k�m@c"�@[C�@Q�@H��@?�w@:�@4j@.�+@(Ĝ@"��@ff@�7@(�@G�@p�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111   A�G�A�?}A�+A�oA�A���Aϧ�AυA�p�A�O�A�(�A� �A�bA�
=A�A���A��A��yA��HA��;A��#A��
A���A���A�ƨA���AξwAθRAβ-Aβ-Aβ-Aΰ!AΧ�AΝ�AΛ�AΙ�AΗ�A΁A��;A̴9A��A���A�ȴA��A©�A���A�VA���A��HA�I�A�(�A�%A�bNA�jA��wA�ZA�dZA�n�A��A��9A���A��A���A�  A�v�A���A��A�ĜA�ĜA���A�7LA���A��wA�?}A�=qA��A��A�O�A��FA��A���A�ĜA�9XA��A��A�A��7A���A�S�A�A��\A�VA��TA�9XA�ȴA�hsA�z�A��9A��DA��yA���A��hA�VA�A�jA�A���A��A�ZA�ffA���A�ZA��#Az�RAt��Aql�An �Ak��Ah�DAe��Ac�mAc\)Ab�DAaVA_�A^5?A\��AZz�AY+AXbNAWVAT��AR�AP�!ANȴAL��AKC�AH�uAF��AE�7AD(�ABz�A@^5A>z�A<�A;`BA:�uA9?}A8(�A7"�A5�#A4��A3VA1
=A/ƨA/XA.-A-hsA-&�A,VA+XA*jA(�A'S�A%?}A$  A#�A"�HA#oA#l�A#K�A"�HA"ffA!dZA �HAO�Ap�AA�A��A�`A-A��AoA^5A^5A�/A��AA
=Al�A�AC�AJA��A�HAJA+A	��A�+At�A�!AƨAXAA?}A ��@�;d@�x�@�+@���@��-@�b@�ȴ@�K�@�v�@�@�5?@��/@�b@�C�@��@���@�$�@�%@�dZ@��y@�G�@�7L@�r�@�;d@�@ۥ�@��@��/@�I�@ם�@�G�@���@ա�@���@ՙ�@֧�@ա�@�I�@٩�@��#@�O�@ؓu@�1'@ם�@֧�@�G�@�l�@ҸR@�J@�V@�K�@Ο�@�O�@�33@��@��;@��@�C�@��y@�Ĝ@��
@�\)@���@¸R@�M�@�$�@���@��
@�+@���@�E�@��@�@��@��T@��#@�hs@��D@�|�@�-@�&�@���@�z�@�Q�@�1'@�dZ@���@�ff@�-@��@��@�dZ@�
=@���@��@�
=@���@���@�Ĝ@�p�@�p�@���@��!@��@�b@��@�/@�/@�&�@�X@�x�@��@���@�p�@��@��@�z�@� �@��P@��@��@��@��P@�l�@��@��H@��H@�ȴ@�-@���@��@��^@�%@�bN@�  @�l�@�@��!@�~�@�-@��@��T@���@���@�X@�G�@�?}@�&�@�%@�V@���@��j@�A�@�1@��@���@���@�t�@�;d@�ȴ@���@�^5@��@���@���@��h@�O�@�%@�V@���@���@���@�A�@�b@���@��w@���@�t�@�
=@�^5@���@�@���@�p�@��@�r�@�1'@�ƨ@��@�M�@�5?@��@��#@���@��@��@���@�Ĝ@��@� �@��@�t�@��y@�ȴ@��R@���@�E�@��@�`B@�7L@��@��@�Ĝ@��j@��9@���@�z�@�j@�1'@���@��@��@�C�@��@�n�@���@���@��7@�`B@�O�@�7L@���@�9X@���@��@�l�@�\)@�S�@�C�@�33@�+@��@�ȴ@��+@�5?@�$�@��@��@��@�/@���@�bN@�A�@�(�@�(�@��@�  @�"�@���@�x�@�O�@��@���@���@�;d@��@���@��7@���@���@��
@�
=@��y@�33@�1@���@��+@�{@��7@��7@�O�@�O�G�O�@�O�@���@}��@r�!@k�m@c"�@[C�@Q�@H��@?�w@:�@4j@.�+@(Ĝ@"��@ff@�7@(�@G�@p�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B�B�B�B�B!�B1'B6FB;dB@�BB�BC�BD�BE�BE�BF�BF�BF�BF�BG�BG�BG�BG�BG�BG�BH�BH�BH�BH�BJ�BJ�BJ�BJ�BK�BL�BL�BO�B^5Bs�B�B�NB�#B�B
=B!�B/B1'B:^B@�BB�BP�BT�BffBjB�B�\B�{B��B��B��B�bB�Bu�B|�B�\B�DB�DB�%B�%B}�B�7B�oB��B��B��B��B��B�'B��B��B�\B�%Bz�Bq�BaHBL�B=qB&�B�BB�`B��B��B�Bm�BZBA�B.B"�B{B
=B
��B
�5B
ŢB
�!B
�VB
{�B
bNB
C�B
1'B
�B
B	��B	��B	�B	n�B	ZB	E�B	33B	%�B	 �B	�B	{B		7B	B��B�B�B�fB�BB��B��BÖB��BɺB��B�?B�B��B��B��B��B��B�VB�+B�B�B�B~�B{�Bw�Br�Bk�BffBcTB^5BYBVBP�BK�BG�BC�B@�B:^B8RB>wB@�BQ�BjBs�Bw�Bz�Bz�Bx�Bn�Be`BcTB]/BVBQ�BP�BT�BYBe`BcTBcTBiyBs�B�1B�bB��B��B�VB�B� By�Bn�BhsBffBe`BdZB\)BQ�BL�BI�BF�BC�BA�BC�BM�BL�BL�BG�BF�BC�B@�BC�BD�BC�BB�BC�BB�BG�BF�BH�BN�BVBYB[#BW
BVBYB[#BbNBe`BcTBhsBs�Bx�B{�B�=B�oB�B�dBBŢBǮBȴB��B��B��B��B��B��B��B��BǮBŢB��B�qB�dB�qB�qB�qB�^B�jB�wB��BBĜBŢB��B��B�
B�)B�HB�`B�B�B�B�B�B�B�B�B�B�B��B��B��B��B	B	B	B	B	B	B	+B	+B	1B	1B	
=B	\B	uB	�B	�B	�B	�B	�B	+B	49B	6FB	8RB	:^B	=qB	A�B	D�B	G�B	L�B	R�B	W
B	ZB	]/B	_;B	`BB	`BB	aHB	aHB	bNB	dZB	e`B	ffB	gmB	jB	jB	jB	k�B	l�B	m�B	p�B	q�B	w�B	y�B	{�B	}�B	� B	� B	� B	�B	�B	�B	�B	�%B	�1B	�7B	�JB	�PB	�hB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�!B	�'B	�3B	�9B	�?B	�?B	�FB	�FB	�LB	�RB	�^B	�^B	�dB	�wB	�}B	��B	��B	��B	B	ÖB	ŢB	ǮB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�
B	�
B	�B	�B	�B	�#B	�#B	�)B	�)B	�)B	�)B	�5B	�BB	�BB	�BB	�BB	�BB	�HB	�HB	�HB	�HB	�NB	�NB	�NB	�NB	�NB	�NB	�NB	�TB	�TB	�ZB	�`B	�`B	�`B	�`B	�ZB	�TB	�BB	�BB	�HB	�HB	�NB	�HB	�/B	�B	�B	�#B	�/B	�#B	�B	�B	�
B	�)B	�HB	�BB	�/B	�#B	�#B	�)B	�)B	�/B	�;B	�B
B
VB
�B
�B
%�B
.B
5?B
<jB
C�B
H�B
L�B
Q�B
W
B
\)B
`BB
e`B
jB
m�B
q�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111   BzB{B{B{B�B�B!�B1B6:B;ZB@yBB�BC�BD�BE�BE�BF�BF�BF�BF�BG�BG�BG�BG�BG�BG�BH�BH�BH�BH�BJ�BJ�BJ�BJ�BK�BL�BL�BO�B^,Bs�B�qB�CB�B�B
0B!�B/B1B:RB@tBB�BP�BT�Bf]BjqB�B�PB�mB��B��B��B�UB�Bu�B|�B�NB�6B�8B�B�B}�B�'B�dB��B��B��B��B��B�B��B��B�MB�Bz�Bq�Ba9BL�B=gB&�B�BB�TB�wB��B�Bm�BZBA}B.B"�BpB
0B
��B
�%B
œB
�B
�JB
{�B
bAB
C�B
1B
�B
B	��B	��B	�B	n�B	ZB	E�B	3$B	%�B	 �B	�B	lB		'B	B��B�B�oB�WB�4B��BʰBÇB��BɩB�xB�/B��B��B��B��B��B�qB�GB�B�B��B��B~�B{�Bw�Br�BksBfXBcDB^&BY
BU�BP�BK�BG�BC�B@rB:MB8CB>gB@sBQ�BjoBs�Bw�Bz�Bz�Bx�Bn�BePBcCB] BU�BQ�BP�BT�BYBePBcBBcCBijBs�B�B�QB��B��B�DB��B�By�Bn�BhbBfWBeNBdGB\BQ�BL�BI�BF�BC�BAwBC�BM�BL�BL�BG�BF�BC�B@rBC�BD�BC�BB|BC�BB|BG�BF�BH�BN�BU�BYB[BV�BU�BYB[Bb?BeOBcCBh`Bs�Bx�B{�B�+B�[B��B�PB�}BŒBǞBȢBʰB̻B��B��B��BͿB̻BʯBǚBőB�wB�_B�SB�]B�]B�aB�LB�XB�eB�nB�~BĈBŐB˴B��B��B�B�5B�MB�yB�B�B�B�B�B�B�B�B�B��B��B��B��B	 �B	�B	�B	�B	B	B	B	B	B	 B	
+B	IB	dB	�B	�B	�B	�B	�B	*�B	4&B	64B	8@B	:IB	=]B	AwB	D�B	G�B	L�B	R�B	V�B	Z
B	]B	_+B	`0B	`0B	a5B	a4B	b;B	dIB	eOB	fRB	g[B	jjB	jnB	jnB	ksB	lyB	m�B	p�B	q�B	w�B	y�B	{�B	}�B	�B	�B	�B	��B	� B	�B	�B	�B	�B	�%B	�:B	�>B	�WB	�]B	�dB	�hB	�pB	�uB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	� B	�&B	�.B	�-B	�4B	�4B	�:B	�?B	�LB	�KB	�QB	�dB	�hB	�rB	�vB	�vB	�|B	ÅB	ŏB	ǜB	ǚB	ȡB	ʱB	˵B	˴B	ͿB	ͿB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�B	�#B	�0B	�/B	�/B	�3B	�0B	�6B	�5B	�4B	�6B	�;B	�;B	�=B	�:B	�<B	�9B	�=B	�@B	�DB	�HB	�OB	�NB	�OB	�MB	�FB	�@B	�/B	�.B	�5B	�6B	�9B	�7B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	�6B	�0B	�B	�B	�B	�B	�B	�G�O�B	�~B
B
BB
|B
�B
%�B
-�B
5.B
<XB
C�B
H�B
L�B
Q�B
V�B
\B
`0B
eLB
jmB
mB
q�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.14 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             201904081332482019040813324820190408133248  AO  ARCAADJP                                                                    20181121125830    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181121125830  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181121125830  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190408133248  IP                  G�O�G�O�G�O�                