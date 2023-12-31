CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-21T12:59:53Z creation      
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
_FillValue                    �,Argo profile    3.1 1.2 19500101000000  20181121125953  20190405100759  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  5287                            2C  D   APEX                            6529                            072314                          846 @��K�f�K1   @��Lq�"�@/~��"���dQ����1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   A   A   @���@�  @���AffA@  Aa��A���A�33A�ffA�33A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  C   C  C�C�fC�fC
  C  C  C  C  C  C  C  C  C  C  C   C!�fC$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:�C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	y�D	��D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dts3Dy� D�#3D�P D���D���D��D�I�D�y�D���D�	�D�P D�� D��fD�fD�  D�` D�3D��D�<�D�s3D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�@���A�HA&�GAHz�Aj{A��A�p�A���A�p�A�=qA�=qA�=qA�=qB�B
�B�B�B"�B*�B2�B:�BB�BJ�BR�BZ�Bb�Bj�Br�Bz�B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�B�B�\B�\B�\B�\B�\B�\C ��C��C�HCnCnC
��C��C��C��C��C��C��C��C��C��C��C ��C"nC$��C&��C(��C*��C,��C.��C0��C2��C4��C6��C8��C:�HC<��C>��C@��CB��CD��CF��CH��CJ��CL��CN��CP��CR��CT��CV��CX��CZ��C\��C^��C`��Cb��Cd��Cf��Ch��Cj��Cl��Cn��Cp��Cr��Ct��Cv��Cx��Cz��C|��C~��C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�P�C�C�C�C�C�C�C�P�C�P�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�7
C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�D !�D ��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D	!�D	��D
�D
��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D !�D ��D!!�D!��D"!�D"��D#!�D#��D$!�D$��D%!�D%��D&!�D&��D'!�D'��D(!�D(��D)!�D)��D*!�D*��D+!�D+��D,!�D,��D-!�D-��D.!�D.��D/!�D/��D0!�D0��D1!�D1��D2!�D2��D3!�D3��D4!�D4��D5!�D5��D6!�D6��D7!�D7��D8!�D8��D9!�D9��D:!�D:��D;!�D;��D<!�D<��D=!�D=��D>!�D>��D?!�D?��D@!�D@��DA!�DA��DB!�DB��DC!�DC��DD!�DD��DE!�DE��DF!�DF��DG!�DG��DH!�DH��DI!�DI��DJ!�DJ��DK!�DK��DL!�DL��DM!�DM��DN!�DN��DO!�DO��DP!�DP��DQ!�DQ��DR!�DR��DS!�DS��DT!�DT��DU!�DU��DV!�DV��DW!�DW��DX!�DX��DY!�DY��DZ!�DZ��D[!�D[��D\!�D\��D]!�D]��D^!�D^��D_!�D_��D`!�D`��Da!�Da��Db!�Db��Dc!�Dc��Dd!�Dd��De!�De��Df!�Df��Dg!�Dg��Dh!�Dh��Di!�Di��Dj!�Dj��Dk!�Dk��Dl!�Dl��Dm!�Dm��Dn!�Dn��Do!�Do��Dp!�Dp��Dq!�Dq��Dr!�Dr��Ds!�Ds��Dt!�Dt�Dy��D�4)D�`�D���D���D�*�D�Z�D���D���D��D�`�D���D��\D�\D�0�D�p�D��)D���D�M�D�)D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�A���AϾwAϥ�A�=qA���A΋DA΃A�z�Aΰ!A�|�AН�A���AоwAжFAЩ�AЗ�A�hsA�ZA�A�A�"�A�
=A���A��A��yA��`A��TA��;A��yA��mA��yA��A��A���A���A��A���A�%A�%A�1A�
=A�oA��A�$�A�1'A�+A�&�A�&�A�(�A�(�A�$�A�$�A�$�A�$�A�$�A�"�A� �A��A��A�bNA�p�A�$�A�A���A�z�A�G�A�/A���A��`A���A�5?A��uA���A�`BA�(�A���A��7A�bA���A�A�S�A��+A�&�A�1A�z�A�A�r�A��9A�^5A�VA�ƨA�ȴA��mA�A��!A���A��A�=qA��hA���A��hA�+A�n�A�&�A��DA}�mAy�AxȴAxZAvI�Arr�AmdZAjAd��Ac�AcK�Ab �A^^5AY��AX�AUhsAS��AQ�#ANffAK�AI��AH�`AF9XABZAA��A@{A>�A=��A=`BA<��A9�-A9
=A9��A9K�A8��A7��A7l�A7�A7K�A7�7A7;dA6�RA5��A4=qA3S�A1�A/�A.�HA-�#A,(�A+��A)33A)XA)�A(�A&$�A%C�A#�#A"�A!hsA r�A^5A�A��AC�A��A�wA��A�DA�#A33AĜAĜA�A�AI�A5?A�A�A\)A�A��AJA��A1'A�#AVA�HAM�A$�A�^A��A33An�A  A
��A	K�A�uAK�A�^AbNAhsA��A�7A �A �`A ��A �uA 5?@��
@�@�^5@�Ĝ@�ƨ@��@���@�&�@��@���@���@�=q@��@�G�@�@�l�@�33@�ȴ@�V@��@��@�V@�t�@���@���@홚@�$�@��T@�p�@�O�@�&�@��@��y@� �@�z�@�M�@�V@�@�ƨ@�w@�|�@�S�@�S�@���@���@�Z@�%@���@�5?@�1@ߍP@�&�@��T@◍@◍@�J@⟾@�+@�M�@�5?@��@�-@ᙚ@�?}@�Z@�(�@߶F@�C�@�
=@޸R@���@�X@��@ܼj@�|�@��@���@ە�@��;@���@۶F@۶F@�\)@�
=@�ȴ@�ff@��T@�%@�b@���@�l�@�33@�n�@�$�@��T@պ^@�V@�Q�@�t�@�ȴ@�V@ѩ�@�?}@�%@У�@�1@϶F@�dZ@��@�ff@�-@���@�X@�Ĝ@� �@��
@�t�@�o@��@ʸR@�v�@��@�X@�/@ț�@ȃ@�I�@Ǿw@�dZ@�C�@�"�@Ə\@�@ŉ7@��@Ĭ@��@å�@��@�v�@���@�7L@�9X@��P@�S�@�K�@�"�@�
=@���@��@��@�ȴ@���@�n�@���@���@�V@��j@��@��D@�Q�@�1@���@���@�5?@��^@�?}@���@��F@���@�~�@�=q@��@���@�`B@���@�9X@�  @��P@�ȴ@�-@��T@��-@�/@�1@��
@��F@�C�@�E�@��^@�x�@�`B@�/@�I�@�ƨ@���@�33@��@�ȴ@���@�J@���@��h@�`B@�O�@��/@�Z@�(�@��F@���@��@�\)@�"�@���@���@�v�@�$�@��@���@��7@�p�@�O�@��@���@�I�@��@���@�dZ@�33@��y@�V@�E�@�-@���@��`@�Ĝ@���@�A�@�ƨ@�C�@�+@�ȴ@��\@�~�@�^5@��@���@��T@��#@��-@��7@�`B@���@�A�@�b@���@�ƨ@��@��@�@��!@�v�@�E�@�$�@�@��-@�`B@�&�@��@��j@��D@�r�@�Q�@�b@�&�@��@��9@���@�b@v�R@kƨ@`1'@X �@PQ�@Dz�@=V@4�/@0Q�@+S�@%�-@ ��@V@�P@-@p�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�A���AϾwAϥ�A�=qA���A΋DA΃A�z�Aΰ!A�|�AН�A���AоwAжFAЩ�AЗ�A�hsA�ZA�A�A�"�A�
=A���A��A��yA��`A��TA��;A��yA��mA��yA��A��A���A���A��A���A�%A�%A�1A�
=A�oA��A�$�A�1'A�+A�&�A�&�A�(�A�(�A�$�A�$�A�$�A�$�A�$�A�"�A� �A��A��A�bNA�p�A�$�A�A���A�z�A�G�A�/A���A��`A���A�5?A��uA���A�`BA�(�A���A��7A�bA���A�A�S�A��+A�&�A�1A�z�A�A�r�A��9A�^5A�VA�ƨA�ȴA��mA�A��!A���A��A�=qA��hA���A��hA�+A�n�A�&�A��DA}�mAy�AxȴAxZAvI�Arr�AmdZAjAd��Ac�AcK�Ab �A^^5AY��AX�AUhsAS��AQ�#ANffAK�AI��AH�`AF9XABZAA��A@{A>�A=��A=`BA<��A9�-A9
=A9��A9K�A8��A7��A7l�A7�A7K�A7�7A7;dA6�RA5��A4=qA3S�A1�A/�A.�HA-�#A,(�A+��A)33A)XA)�A(�A&$�A%C�A#�#A"�A!hsA r�A^5A�A��AC�A��A�wA��A�DA�#A33AĜAĜA�A�AI�A5?A�A�A\)A�A��AJA��A1'A�#AVA�HAM�A$�A�^A��A33An�A  A
��A	K�A�uAK�A�^AbNAhsA��A�7A �A �`A ��A �uA 5?@��
@�@�^5@�Ĝ@�ƨ@��@���@�&�@��@���@���@�=q@��@�G�@�@�l�@�33@�ȴ@�V@��@��@�V@�t�@���@���@홚@�$�@��T@�p�@�O�@�&�@��@��y@� �@�z�@�M�@�V@�@�ƨ@�w@�|�@�S�@�S�@���@���@�Z@�%@���@�5?@�1@ߍP@�&�@��T@◍@◍@�J@⟾@�+@�M�@�5?@��@�-@ᙚ@�?}@�Z@�(�@߶F@�C�@�
=@޸R@���@�X@��@ܼj@�|�@��@���@ە�@��;@���@۶F@۶F@�\)@�
=@�ȴ@�ff@��T@�%@�b@���@�l�@�33@�n�@�$�@��T@պ^@�V@�Q�@�t�@�ȴ@�V@ѩ�@�?}@�%@У�@�1@϶F@�dZ@��@�ff@�-@���@�X@�Ĝ@� �@��
@�t�@�o@��@ʸR@�v�@��@�X@�/@ț�@ȃ@�I�@Ǿw@�dZ@�C�@�"�@Ə\@�@ŉ7@��@Ĭ@��@å�@��@�v�@���@�7L@�9X@��P@�S�@�K�@�"�@�
=@���@��@��@�ȴ@���@�n�@���@���@�V@��j@��@��D@�Q�@�1@���@���@�5?@��^@�?}@���@��F@���@�~�@�=q@��@���@�`B@���@�9X@�  @��P@�ȴ@�-@��T@��-@�/@�1@��
@��F@�C�@�E�@��^@�x�@�`B@�/@�I�@�ƨ@���@�33@��@�ȴ@���@�J@���@��h@�`B@�O�@��/@�Z@�(�@��F@���@��@�\)@�"�@���@���@�v�@�$�@��@���@��7@�p�@�O�@��@���@�I�@��@���@�dZ@�33@��y@�V@�E�@�-@���@��`@�Ĝ@���@�A�@�ƨ@�C�@�+@�ȴ@��\@�~�@�^5@��@���@��T@��#@��-@��7@�`B@���@�A�@�b@���@�ƨ@��@��@�@��!@�v�@�E�@�$�@�@��-@�`B@�&�@��@��j@��D@�r�@�Q�@�b@�&�@��@��9@���@�b@v�R@kƨ@`1'@X �@PQ�@Dz�@=V@4�/@0Q�@+S�@%�-@ ��@V@�P@-@p�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B�B�B�B��B��B��BDB�%B
�B
��B�B'�B)�B+B+B)�B)�B(�B$�B"�B �B�B�B�B �B!�B$�B%�B&�B&�B(�B,B,B,B/B33B5?B6FB8RB;dB=qBA�BC�BC�BB�BC�BD�BD�BD�BD�BD�BD�BD�BD�BD�BD�BA�B,B+BB8RBv�B��B�B�
B��BB  B�B.B5?B:^B<jBE�BF�BG�BF�BC�BC�BE�BF�BH�BG�B7LB'�BB�B�/B��BÖB�RB��B��B[#BuB
�HB
��B
q�B
\)B
F�B
/B
\B	��B	�`B	ɺB	�}B	�XB	��B	�{B	}�B	jB	P�B	I�B	D�B	;dB	-B	�B	�B	{B	uB	oB	VB	B	  B	B��B��B	B	�B	&�B	2-B	:^B	8RB	0!B	8RB	W
B	}�B	�DB	��B	�B	�RB	��B	��B	��B	��B	��B	��B	��B	�qB	�!B	��B	��B	�#B	�/B	��B	��B	�
B	�B	��B	��B	��B	�RB	�B	��B	��B	�hB	�PB	�B	�B	�B	� B	�B	�B	� B	�B	�+B	�=B	�oB	�{B	�uB	�uB	��B	��B	��B	��B	��B	�\B	�JB	�PB	�VB	�bB	�JB	�DB	�1B	� B	|�B	x�B	u�B	m�B	_;B	[#B	T�B	O�B	G�B	A�B	>wB	:^B	<jB	>wB	@�B	B�B	F�B	J�B	M�B	R�B	P�B	O�B	R�B	VB	W
B	XB	XB	VB	W
B	YB	[#B	\)B	`BB	iyB	l�B	l�B	k�B	jB	k�B	k�B	m�B	o�B	q�B	}�B	�%B	�B	�B	�B	�B	�B	z�B	}�B	�=B	�hB	�DB	�+B	�1B	�=B	�JB	�VB	�=B	�+B	�bB	��B	��B	�uB	�DB	�PB	��B	��B	��B	��B	��B	�B	�-B	�3B	�9B	�?B	�FB	�XB	�dB	�wB	�}B	��B	��B	�}B	�}B	ÖB	ĜB	ĜB	ĜB	ŢB	ɺB	��B	��B	�B	�#B	�)B	�/B	�/B	�5B	�/B	�/B	�/B	�/B	�)B	�)B	�/B	�;B	�BB	�;B	�BB	�TB	�ZB	�`B	�`B	�fB	�fB	�fB	�fB	�fB	�mB	�sB	�sB	�sB	�sB	�sB	�sB	�sB	�sB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
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
1B
1B
	7B
	7B
	7B
1B
	7B
	7B
	7B
	7B

=B

=B
DB
DB

=B
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
DB
DB
DB
DB
JB
JB
JB
VB
VB
\B
VB
\B
\B
\B
\B
VB
VB
VB
VB
VB
\B
\B
\B
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
bB
uB
�B
�B
#�B
,B
33B
9XB
?}B
F�B
M�B
M�B
O�B
VB
ZB
^5B
dZB
iyB
m�B
q�B
u�B
y�B
~�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�xB�zB�zB�uB�B��B��B��BB��B
��B
��B�B'�B)�B*�B*�B)�B)�B(�B$�B"�B �B�B�B�B �B!�B$�B%�B&�B&�B(�B+�B+�B+�B.�B3B5B6B8%B;;B=GBA^BClBClBBeBCkBDsBDsBDpBDpBDpBDpBDsBDsBDpBDpBA_B+�BB�B8#Bv�B�|B��B��B��B �B��BXB-�B5B:.B<<BEtBFuBG�BFvBCkBCiBEtBFzBH�BG�B7B'�B�B�fB��B͢B�cB�B��B�SBZ�BBB
�B
��B
qxB
[�B
FtB
.�B
'B	��B	�)B	ɅB	�IB	�$B	��B	�EB	}�B	jFB	P�B	I�B	DeB	;-B	,�B	_B	wB	CB	<B	8B	B	�B��B	�B��B��B	�B	}B	&�B	1�B	:"B	8B	/�B	8B	V�B	}�B	�B	�uB	��B	�B	�KB	͖B	̑B	ˊB	ʆB	ΜB	ҶB	�2B	��B	ӻB	ӻB	��B	��B	ʅB	ѭB	��B	��B	ҳB	̑B	�MB	�B	��B	��B	�OB	�,B	�B	��B	��B	��B	�B	��B	��B	�B	��B	��B	��B	�0B	�?B	�7B	�6B	�PB	�TB	�]B	�aB	�SB	�B	�	B	�B	�B	�"B	�B	�B	��B	�B	|�B	x�B	u�B	mRB	^�B	Z�B	T�B	O�B	GjB	AGB	>8B	:B	<)B	>6B	@AB	BMB	FiB	J�B	M�B	R�B	P�B	O�B	R�B	U�B	V�B	W�B	W�B	U�B	V�B	X�B	Z�B	[�B	` B	i8B	lIB	lIB	kCB	j?B	kCB	kDB	mNB	o\B	qhB	}�B	��B	��B	��B	��B	��B	��B	z�B	}�B	��B	�%B	�B	��B	��B	��B	�B	�B	��B	��B	� B	�WB	�QB	�3B	�B	�B	�\B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�"B	�5B	�<B	�BB	�AB	�<B	�;B	�SB	�YB	�WB	�ZB	�_B	�vB	˂B	ӶB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	� B	�B	�B	�B	�B	�"B	�!B	�$B	�%B	�#B	�+B	�1B	�1B	�/B	�0B	�0B	�1B	�-B	�0B	�5B	�8B	�6B	�4B	�<B	�=B	�=B	�>B	�=B	�=B	�;B	�AB	�?B	�BB	�BB	�IB	�FB	�BB	�FB	�KB	�NB	�LB	�LB	�TB	�TB	�ZB	�\B	�ZB	�XB	�gB	�fB	�mB	�lB	�kB	�mB	�lB	�oB	�jB	�mB	�kB	�nB	�lB	�sB	�rB	�xB	�zB	�xB	�wB	�vB	�wB	�{B	��B	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	�B
	�B

�B

�B
	�B
 B

�B

�B

�B
 B
B
B
B
B
B

�B

�B
 B

�B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
1B
>B
lB
#�B
+�B
2�B
9B
?6B
FdB
M�B
M�B
O�B
U�B
Y�B
]�B
dB
i4B
mNB
qdB
u~B
y�B
~�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.53 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             201904051007592019040510075920190405100759  AO  ARCAADJP                                                                    20181121125953    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181121125953  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181121125953  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190405100759  IP                  G�O�G�O�G�O�                