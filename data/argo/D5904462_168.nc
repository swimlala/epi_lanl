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
_FillValue                    �,Argo profile    3.1 1.2 19500101000000  20181121125953  20190405100759  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  5287                            2C  D   APEX                            6529                            072314                          846 @��6͝X1   @���b�\@/�hr�!�dAG�z�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   A   A   @�33@�  A   A   AA��Aa��A�  A�  A�  A�  A�33A�33A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B���B�  B�  C   C  C�C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4�C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch�Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~�C��C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dts3Dys3D�	�D�<�D�� D�ɚD�fD�,�D�� D�ɚD��D�L�D�` D��3D���D�6fDڌ�D��fD���D�&fD�i�D�\�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�(�@���Az�A(z�AJ{Aj{A�=qA�=qA�=qA�=qA�p�A�p�A�=qA�=qB�B
�B�B�B"�B*�B2�B:�BB�BJ�BR�BZ�Bb�Bj�Br�Bz�B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�B�B�\B�\B�\B�\B��)B�\B�\C ��C��C�HC��C��C
��C��C��C��C��C��C��C��C��C��C��C ��C"��C$��C&��C(��C*��C,��C.��C0��C2��C4�HC6��C8��C:��C<��C>��C@��CB��CD��CF��CH��CJ��CL��CN��CP��CR��CT��CV��CX��CZ��C\��C^��C`��Cb��Cd��Cf��Ch�HCj��Cl��Cn��Cp��Cr��Ct��Cv��Cx��Cz��C|��C~�HC�P�C�7
C�7
C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�P�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�7
C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�D !�D ��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D	!�D	��D
!�D
��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D !�D ��D!!�D!��D"!�D"��D#!�D#��D$!�D$��D%!�D%��D&!�D&��D'!�D'��D(!�D(��D)!�D)��D*!�D*��D+!�D+��D,!�D,��D-!�D-��D.!�D.��D/!�D/��D0!�D0��D1!�D1��D2!�D2��D3!�D3��D4!�D4��D5!�D5��D6!�D6��D7!�D7��D8!�D8��D9!�D9��D:!�D:��D;!�D;��D<!�D<��D=!�D=��D>!�D>��D?!�D?��D@!�D@��DA!�DA��DB!�DB��DC!�DC��DD!�DD��DE!�DE��DF!�DF��DG!�DG��DH!�DH��DI!�DI��DJ!�DJ��DK!�DK��DL!�DL��DM!�DM��DN!�DN��DO!�DO��DP!�DP��DQ!�DQ��DR!�DR��DS!�DS��DT!�DT��DU!�DU��DV!�DV��DW!�DW��DX!�DX��DY!�DY��DZ!�DZ��D[!�D[��D\!�D\��D]!�D]��D^!�D^��D_!�D_��D`!�D`��Da!�Da��Db!�Db��Dc!�Dc��Dd!�Dd��De!�De��Df!�Df��Dg!�Dg��Dh!�Dh��Di!�Di��Dj!�Dj��Dk!�Dk��Dl!�Dl��Dm!�Dm��Dn!�Dn��Do!�Do��Dp!�Dp��Dq!�Dq��Dr!�Dr��Ds!�Ds��Dt!�Dt�Dy�D��D�M�D���D�ڐD�\D�=�D���D�ڐD��D�]�D�p�D��)D��D�G\Dڝ�D��\D��D�7\D�z�D�m�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�VA��A��A� �A�$�A�(�A�$�A��A��A��A�VAξwAΩ�A�ƨAθRAΟ�A�x�A�\)A�K�A�1'A�-A�$�A�5?A�E�A�7LA�VA�A��yA���A͡�A͍PA�r�A�^5A�Q�A�E�A�=qA�7LA�33A�"�A��A̗�A�VA˙�A�&�A�  Aʟ�A��A�ƨA�&�A�`BA�;dA�{A��AǅA��A�ĜA��A��#A�G�Aú^A�Q�A��A��^A�VA��jA�^5A��\A�bA��9A���A��A��!A��!A��jA��A�%A�XA��A���A�XA�7LA��-A���A���A���A�l�A�\)A�=qA�K�A�K�A�+A�7LA��9A��A��A��A�^5A��A�oA�bA�t�A��
A���A�A�?}A�A�A�1A�=qA�v�A��A�mA}�mAzA�Axv�Aq��ApM�Al�Af1'Ac��A`{A]��A\��AX��AO�AH��AE�PAB��A@��A<�A8�\A5\)A3�wA1�wA0�A-dZA+�A*�HA'l�A'oA&^5A%S�A#�wA#oA"�`A"�A"E�A"E�A"��A"ZA"A�A"  A!�7A!XA ��A�TA�yA/A��AO�A;dA�A�HA��A�A�-Al�A"�A�uAJA��A"�A�/A��A&�Ar�A�mAt�AA�AXA33AVA&�A�AA�A^5A�+A9XA�-AhsA
��A	��A��A�FA�!Ar�A��AffAA�A�A �A{A�wA�wA��AA �+A ffA ^5@�|�@��@�~�@���@��@�
=@���@��@��@�;d@�E�@��7@�(�@�P@�5?@�@�&�@��@�j@�Q�@�1'@�l�@��@��@�7@�r�@��m@�1@� �@�z�@�@�?}@�X@��-@�/@�Q�@��@�33@��y@�R@ꟾ@ꗍ@�M�@��@�^@�X@��`@�j@�C�@��@���@�x�@�?}@�O�@�%@�I�@�w@�t�@�C�@�R@�=q@��T@�G�@���@�b@�l�@��@�E�@�Z@�1@�ƨ@��@�{@�x�@�G�@�Ĝ@�Ĝ@�b@��@���@�v�@�-@�@�7L@�bN@���@�dZ@��@�v�@Ѻ^@�x�@�&�@��@�Z@���@�dZ@θR@�p�@�/@�Ĝ@�A�@˶F@�dZ@�;d@�ȴ@�E�@ɲ-@�/@�A�@Ǖ�@�S�@��y@�E�@ũ�@�O�@���@Ý�@�@���@\@�v�@�ff@�5?@��h@�V@�Ĝ@�bN@��@�t�@���@��R@�^5@��-@�p�@�7L@��`@�Ĝ@��D@��
@��P@�dZ@��@��@���@�~�@�E�@�{@��T@�p�@�?}@��@��/@�I�@��;@���@��@�dZ@�33@�J@��@�x�@�G�@��@�z�@�1'@���@���@���@�@��!@�V@�@��h@���@���@�r�@�1@���@�"�@�v�@�-@��T@��^@�x�@�/@��@�Ĝ@�Z@���@�\)@�o@���@�V@��@�p�@��@�Ĝ@�Z@���@��;@�l�@�K�@�o@�ȴ@���@���@���@�v�@�=q@�{@���@���@�7L@��@�9X@���@�l�@���@��R@��+@�n�@�^5@��@���@�x�@�?}@���@���@��u@�r�@� �@�S�@�"�@��\@�M�@�-@�{@���@��@��^@�p�@���@���@��@�bN@�1@��;@��@�dZ@�@��R@�ff@�E�@�-@�@��-@��@�hs@���@��/@��@�9X@�b@��@���@���@��F@���@�dZ@�o@���@��+@�M�@�5?@��@���@���@�G�@��@�r�@���@��\@��@�j@y&�@o�;@hA�@b~�@Wl�@N�y@HA�@@��@97L@1X@+�
@&v�@"^5@@�@n�@E�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�VA��A��A� �A�$�A�(�A�$�A��A��A��A�VAξwAΩ�A�ƨAθRAΟ�A�x�A�\)A�K�A�1'A�-A�$�A�5?A�E�A�7LA�VA�A��yA���A͡�A͍PA�r�A�^5A�Q�A�E�A�=qA�7LA�33A�"�A��A̗�A�VA˙�A�&�A�  Aʟ�A��A�ƨA�&�A�`BA�;dA�{A��AǅA��A�ĜA��A��#A�G�Aú^A�Q�A��A��^A�VA��jA�^5A��\A�bA��9A���A��A��!A��!A��jA��A�%A�XA��A���A�XA�7LA��-A���A���A���A�l�A�\)A�=qA�K�A�K�A�+A�7LA��9A��A��A��A�^5A��A�oA�bA�t�A��
A���A�A�?}A�A�A�1A�=qA�v�A��A�mA}�mAzA�Axv�Aq��ApM�Al�Af1'Ac��A`{A]��A\��AX��AO�AH��AE�PAB��A@��A<�A8�\A5\)A3�wA1�wA0�A-dZA+�A*�HA'l�A'oA&^5A%S�A#�wA#oA"�`A"�A"E�A"E�A"��A"ZA"A�A"  A!�7A!XA ��A�TA�yA/A��AO�A;dA�A�HA��A�A�-Al�A"�A�uAJA��A"�A�/A��A&�Ar�A�mAt�AA�AXA33AVA&�A�AA�A^5A�+A9XA�-AhsA
��A	��A��A�FA�!Ar�A��AffAA�A�A �A{A�wA�wA��AA �+A ffA ^5@�|�@��@�~�@���@��@�
=@���@��@��@�;d@�E�@��7@�(�@�P@�5?@�@�&�@��@�j@�Q�@�1'@�l�@��@��@�7@�r�@��m@�1@� �@�z�@�@�?}@�X@��-@�/@�Q�@��@�33@��y@�R@ꟾ@ꗍ@�M�@��@�^@�X@��`@�j@�C�@��@���@�x�@�?}@�O�@�%@�I�@�w@�t�@�C�@�R@�=q@��T@�G�@���@�b@�l�@��@�E�@�Z@�1@�ƨ@��@�{@�x�@�G�@�Ĝ@�Ĝ@�b@��@���@�v�@�-@�@�7L@�bN@���@�dZ@��@�v�@Ѻ^@�x�@�&�@��@�Z@���@�dZ@θR@�p�@�/@�Ĝ@�A�@˶F@�dZ@�;d@�ȴ@�E�@ɲ-@�/@�A�@Ǖ�@�S�@��y@�E�@ũ�@�O�@���@Ý�@�@���@\@�v�@�ff@�5?@��h@�V@�Ĝ@�bN@��@�t�@���@��R@�^5@��-@�p�@�7L@��`@�Ĝ@��D@��
@��P@�dZ@��@��@���@�~�@�E�@�{@��T@�p�@�?}@��@��/@�I�@��;@���@��@�dZ@�33@�J@��@�x�@�G�@��@�z�@�1'@���@���@���@�@��!@�V@�@��h@���@���@�r�@�1@���@�"�@�v�@�-@��T@��^@�x�@�/@��@�Ĝ@�Z@���@�\)@�o@���@�V@��@�p�@��@�Ĝ@�Z@���@��;@�l�@�K�@�o@�ȴ@���@���@���@�v�@�=q@�{@���@���@�7L@��@�9X@���@�l�@���@��R@��+@�n�@�^5@��@���@�x�@�?}@���@���@��u@�r�@� �@�S�@�"�@��\@�M�@�-@�{@���@��@��^@�p�@���@���@��@�bN@�1@��;@��@�dZ@�@��R@�ff@�E�@�-@�@��-@��@�hs@���@��/@��@�9X@�b@��@���@���@��F@���@�dZ@�o@���@��+@�M�@�5?@��@���@���@�G�@��@�r�@���@��\@��@�j@y&�@o�;@hA�@b~�@Wl�@N�y@HA�@@��@97L@1X@+�
@&v�@"^5@@�@n�@E�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B��BƨB	�oB
jB
ɺB
�/B
�TB
�BB
�/B
�)B
�)B
�/B
�BB
�B
��B
��B
��B
��B
��B
��B
��B
��B  BB+B
=B
=BDBPBJBoB�B�B33BJ�BR�BcTB{�B�DB��B�B�B�!B�3B�LB�jB��BɺB�B�/B�;B�ZB�mB�B�B��B��BB%BPB!�B-B+B'�B'�B �B�B�BbB{B+B+B'�B�BbBB��B��B��B�B�/B��B�!B�DB�B�B{�Bp�B9XB�BVB
�TB
�RB
��B
cTB
"�B
PB
B	��B	��B	�B	�B	�sB	�HB	�B	�?B	��B	��B	{�B	o�B	\)B	K�B	D�B	+B	B�B�`B�TB�BB�/B�/B�5B�5B�TB�BB�B��B��B��BɺB��B�
B�HB�B�B��B	B	�B	-B	6FB	L�B	O�B	P�B	P�B	Q�B	T�B	VB	`BB	m�B	o�B	p�B	r�B	s�B	u�B	x�B	y�B	y�B	y�B	|�B	�7B	�VB	�bB	�\B	�1B	y�B	r�B	m�B	hsB	]/B	T�B	S�B	Q�B	H�B	K�B	P�B	T�B	ZB	^5B	e`B	hsB	e`B	[#B	S�B	L�B	D�B	<jB	7LB	6FB	7LB	8RB	9XB	;dB	@�B	A�B	D�B	H�B	J�B	M�B	T�B	VB	S�B	W
B	XB	]/B	]/B	`BB	dZB	dZB	ffB	ffB	hsB	iyB	iyB	k�B	k�B	k�B	jB	iyB	iyB	iyB	jB	l�B	l�B	l�B	o�B	q�B	v�B	z�B	�B	�B	�JB	�\B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�3B	�LB	�FB	�LB	�dB	�dB	�XB	�^B	�LB	�jB	��B	ĜB	ƨB	ǮB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	�B	�#B	�B	�B	�B	�B	�B	�
B	�B	�#B	�#B	�BB	�BB	�NB	�TB	�TB	�TB	�ZB	�ZB	�ZB	�ZB	�TB	�TB	�ZB	�`B	�`B	�`B	�`B	�`B	�`B	�`B	�`B	�mB	�mB	�mB	�sB	�sB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
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
+B
+B
+B
+B
+B
1B
1B
1B
1B
	7B
	7B
	7B
	7B

=B

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
JB
JB
PB
PB
PB
PB
VB
\B
\B
\B
\B
\B
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
hB
hB
hB
hB
hB
hB
hB
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
�B
�B
�B
&�B
,B
33B
7LB
<jB
A�B
H�B
L�B
N�B
P�B
Q�B
YB
^5B
cTB
hsB
l�B
p�B
v�B
z�B
~�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�aB�iB�fB�iB�vB�{B�wB�[B�YB�]B�|B	�EB
jSB
ɍB
�B
�(B
�B
�B
��B
��B
�B
�B
�TB
�B
��B
��B
��B
��B
��B
��B
��B
��B�B B
B
BB$B!BDBSB�B3	BJ�BR�Bc+B{�B�B�oB��B��B��B�B� B�<B�^BɐB��B�B�B�/B�?B�XB�vB��B��B�B�B!B!�B,�B*�B'�B'�B �BkBYB1BLB*�B*�B'�BlB0B�B��B��B��B�RB��B˗B��B�B��B��B{�BpsB9'ByB#B
�!B
�B
�mB
c"B
"�B
B
�B	��B	��B	�B	�lB	�>B	�B	��B	�
B	��B	�\B	{�B	odB	[�B	K�B	DdB	*�B	�B�ZB�&B�B�B��B��B��B��B�B�B��BШBΚBʄB�zB̏B��B�B�LB�xB��B	�B	^B	,�B	6B	L�B	O�B	P�B	P�B	Q�B	T�B	U�B	`B	mSB	o_B	pdB	rqB	sxB	u�B	x�B	y�B	y�B	y�B	|�B	��B	�B	�"B	�B	��B	y�B	rrB	mOB	h3B	\�B	T�B	S�B	Q�B	HuB	K�B	P�B	T�B	Y�B	]�B	e"B	h3B	e!B	Z�B	S�B	L�B	D[B	<)B	7B	6B	7B	8B	9B	;#B	@BB	AJB	D[B	HrB	J�B	M�B	T�B	U�B	S�B	V�B	W�B	\�B	\�B	`B	dB	dB	f$B	f"B	h2B	i7B	i8B	kBB	kDB	kAB	j=B	i6B	i9B	i7B	j;B	lJB	lIB	lKB	o]B	qgB	v�B	z�B	��B	��B	�B	�B	�@B	�cB	�mB	�wB	�tB	�|B	��B	��B	��B	��B	�	B	�B	�
B	�!B	�!B	�B	�B	�B	�(B	�EB	�[B	�fB	�lB	�oB	�rB	�xB	�~B	�}B	͑B	ФB	ѧB	ӶB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�*B	�+B	�*B	�0B	�0B	�/B	�6B	�7B	�7B	�=B	�=B	�BB	�BB	�CB	�?B	�GB	�GB	�FB	�FB	�TB	�UB	�[B	�ZB	�\B	�ZB	�[B	�[B	�`B	�_B	�^B	�fB	�fB	�hB	�gB	�nB	�sB	�rB	�rB	�sB	�oB	�qB	�wB	�xB	�yB	�B	�~B	�B	�}B	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
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
	�B
	�B

�B

�B

�B
 B
 B
B

�B

�B

�B
 B
B
B
B
B
B
B
B
B
B
B
B

B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
#B
$B
"B
#B
#B
#B
$B
*B
)B
2B
5B
4B
7B
5B
4B
7B
=B
OB
LB
&�B
+�B
2�B
7B
<&B
ACB
HpB
L�B
N�B
P�B
Q�B
X�B
]�B
cB
h/B
lHB
paB
v�B
z�B
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