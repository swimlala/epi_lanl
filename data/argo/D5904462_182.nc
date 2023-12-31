CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-21T12:59:57Z creation      
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
_FillValue                    �,Argo profile    3.1 1.2 19500101000000  20181121125957  20190405100801  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  5287                            2C  D   APEX                            6529                            072314                          846 @����1   @���H�@.��Q��c�^5?}1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   A   A   @�33@�  A   A   A@  A`  A���A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  BhffBpffBx  B��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C�C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn�Cp�Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  DtS3Dy�3D�3D�I�D��fD�� D�3D�L�D�� D�� D�fD�6fD���D�ɚD�  D�9�D�l�D�ɚD�3D�)�D�vfD��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�(�@���Az�A(z�AHz�Ahz�A�
>A�=qA�=qA�=qA�=qA�=qA�=qA�=qB�B
�B�B�B"�B*�B2�B:�BB�BJ�BR�BZ�Bb�Bj�Br�Bz�B��)B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\C ��C��C��C��C��C
��C��C��C��C��C��C��C��C�HC��C��C ��C"��C$��C&��C(��C*��C,��C.��C0��C2��C4��C6��C8��C:��C<��C>��C@��CB��CD��CF��CH��CJ��CL��CN��CP��CR��CT��CV��CX��CZ��C\��C^��C`��Cb��Cd��Cf��Ch��Cj��Cl��Cn�HCp�HCr��Ct��Cv��Cx��Cz��C|��C~��C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�D !�D ��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D	!�D	��D
!�D
��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D !�D ��D!!�D!��D"!�D"��D#!�D#��D$!�D$��D%!�D%��D&!�D&��D'!�D'��D(!�D(��D)!�D)��D*!�D*��D+!�D+��D,!�D,��D-!�D-��D.!�D.��D/!�D/��D0!�D0��D1!�D1��D2!�D2��D3!�D3��D4!�D4��D5!�D5��D6!�D6��D7!�D7��D8!�D8��D9!�D9��D:!�D:��D;!�D;��D<!�D<��D=!�D=��D>!�D>��D?!�D?��D@!�D@��DA!�DA��DB!�DB��DC!�DC��DD!�DD��DE!�DE��DF!�DF��DG!�DG��DH!�DH��DI!�DI��DJ!�DJ��DK!�DK��DL!�DL��DM!�DM��DN!�DN��DO!�DO��DP!�DP��DQ!�DQ��DR!�DR��DS!�DS��DT!�DT��DU!�DU��DV!�DV��DW!�DW��DX!�DX��DY!�DY��DZ!�DZ��D[!�D[��D\!�D\��D]!�D]��D^!�D^��D_!�D_��D`!�D`��Da!�Da��Db!�Db��Dc!�Dc��Dd!�Dd��De!�De��Df!�Df��Dg!�Dg��Dh!�Dh��Di!�Di��Dj!�Dj��Dk!�Dk��Dl!�Dl��Dm!�Dm��Dn!�Dn��Do!�Do��Dp!�Dp��Dq!�Dq��Dr!�Dr��Ds!�Ds��Dt!�DtuDy�D�)D�Z�D��\D���D�$)D�]�D���D���D�\D�G\D���D�ڐD��D�J�D�}�D�ڐD�)D�:�D�\D��\11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�A�1A�bA�VA�VA�
=A�{A��A��A��A��A��A��A��A�oA��yA��#A��HA��A���A�ȴA�Aͣ�A͡�A͝�A͝�A͛�A͛�A͙�A͕�A�|�A��TA�XA��mA�ZA��A�9XA��`A�dZA�M�A�9XA�$�A�O�A�%AżjA�XA��#A�hsAř�Aś�Ař�AőhA�z�A�"�A��TA�oA���A��mA���A�r�A���A�A�S�A��-A��A���A���A���A��A��7A��FA�l�A�ĜA�?}A�;dA��A���A�ƨA�;dA�oA�9XA��DA��A�M�A�A���A�E�A� �A��A�l�A�ƨA�VA�  A�bNA�z�A�
=A��#A�Q�A|�A{�FAz(�Ax�At�RAr�Aq��Aq"�AodZAmoAj5?AgAc�A`��A_7LA]+A\�A[K�AZ9XAX��AWƨAW�AT�ARVAO/AK��AI�TAH��AGoAD�AB5?A>�A=S�A<1A:n�A9VA7�A4��A3�wA1�^A0A�A.��A.{A-K�A,r�A+��A*r�A)oA't�A&�!A'%A'%A&ffA&bNA&ZA&E�A&�jA';dA&^5A&$�A%G�A#��A!t�A��A&�A�
A�A ��A!�
A!�TA �uA�
AA33Ap�A
=AffA�A�AȴA1'A\)A�-AM�A�`A�RA �A��A?}A�mA+A
M�A
bNA
jA
n�A
1A	��A	��A	�7A��A��AK�A�+A��AoA�jAz�A\)A�9AA�A-A9XAA\)A
=A ��A ��A �9@�\)@�M�@��^@�X@�1@���@�S�@��@�?}@�p�@�j@�p�@�7L@�@��#@�@@�E�@�{@�X@�?}@�@�1@�t�@�"�@���@�V@�=q@���@��@�`B@�G�@߮@ޗ�@�=q@�E�@��@�v�@ޟ�@ާ�@�V@ݺ^@�hs@�hs@�x�@ݩ�@�/@�9X@ە�@�
=@ڏ\@�$�@��T@��T@ف@�Ĝ@�bN@���@�C�@֗�@��#@��`@ԋD@� �@��@Ӯ@�K�@��@Ұ!@���@�hs@��/@���@Ѓ@�;d@·+@�-@���@�{@́@̓u@��
@��@�n�@�5?@��@�x�@�?}@ȓu@�K�@�J@ēu@�  @�ƨ@�dZ@�ȴ@�~�@���@��@���@��/@��D@��;@�\)@��H@�ff@�$�@�$�@�@�hs@��@��/@��u@�z�@�9X@��@��;@��
@��w@��@�o@�v�@�V@�ȴ@�33@�+@��\@�~�@�;d@���@��\@�n�@�$�@�V@�^5@�@��h@�hs@��@�bN@���@��
@�dZ@��H@�-@�J@�@��^@��@��@� �@���@��
@���@�S�@��@�^5@�$�@��T@���@��#@���@���@���@�`B@�O�@�?}@�%@�Ĝ@�A�@��w@�33@�33@�@��!@��@�%@��u@�j@��@�bN@��@�K�@�v�@�ff@�^5@���@��@��/@�9X@���@�l�@�"�@�@��y@�v�@�@�O�@�V@���@���@�  @��
@�C�@��R@�E�@��T@��7@�G�@��/@��j@��@�9X@��@���@�S�@���@��R@�5?@���@��h@��j@�  @��@�K�@�o@��H@�$�@��^@��@�O�@�/@���@���@�9X@�  @��w@�|�@�C�@�@�ȴ@��!@�v�@�{@���@�hs@�&�@�%@��`@��@��@��/@��@��u@�j@�1@��@�|�@�dZ@�S�@�33@�
=@�v�@�M�@��7@�O�@�&�@���@��u@�j@�Q�@�1'@�=q@�n�@�@z��@p��@hĜ@`��@V�R@L9X@DZ@<�@6��@01'@*�@$�@�@��@�F@  @�F@ �11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�A�1A�bA�VA�VA�
=A�{A��A��A��A��A��A��A��A�oA��yA��#A��HA��A���A�ȴA�Aͣ�A͡�A͝�A͝�A͛�A͛�A͙�A͕�A�|�A��TA�XA��mA�ZA��A�9XA��`A�dZA�M�A�9XA�$�A�O�A�%AżjA�XA��#A�hsAř�Aś�Ař�AőhA�z�A�"�A��TA�oA���A��mA���A�r�A���A�A�S�A��-A��A���A���A���A��A��7A��FA�l�A�ĜA�?}A�;dA��A���A�ƨA�;dA�oA�9XA��DA��A�M�A�A���A�E�A� �A��A�l�A�ƨA�VA�  A�bNA�z�A�
=A��#A�Q�A|�A{�FAz(�Ax�At�RAr�Aq��Aq"�AodZAmoAj5?AgAc�A`��A_7LA]+A\�A[K�AZ9XAX��AWƨAW�AT�ARVAO/AK��AI�TAH��AGoAD�AB5?A>�A=S�A<1A:n�A9VA7�A4��A3�wA1�^A0A�A.��A.{A-K�A,r�A+��A*r�A)oA't�A&�!A'%A'%A&ffA&bNA&ZA&E�A&�jA';dA&^5A&$�A%G�A#��A!t�A��A&�A�
A�A ��A!�
A!�TA �uA�
AA33Ap�A
=AffA�A�AȴA1'A\)A�-AM�A�`A�RA �A��A?}A�mA+A
M�A
bNA
jA
n�A
1A	��A	��A	�7A��A��AK�A�+A��AoA�jAz�A\)A�9AA�A-A9XAA\)A
=A ��A ��A �9@�\)@�M�@��^@�X@�1@���@�S�@��@�?}@�p�@�j@�p�@�7L@�@��#@�@@�E�@�{@�X@�?}@�@�1@�t�@�"�@���@�V@�=q@���@��@�`B@�G�@߮@ޗ�@�=q@�E�@��@�v�@ޟ�@ާ�@�V@ݺ^@�hs@�hs@�x�@ݩ�@�/@�9X@ە�@�
=@ڏ\@�$�@��T@��T@ف@�Ĝ@�bN@���@�C�@֗�@��#@��`@ԋD@� �@��@Ӯ@�K�@��@Ұ!@���@�hs@��/@���@Ѓ@�;d@·+@�-@���@�{@́@̓u@��
@��@�n�@�5?@��@�x�@�?}@ȓu@�K�@�J@ēu@�  @�ƨ@�dZ@�ȴ@�~�@���@��@���@��/@��D@��;@�\)@��H@�ff@�$�@�$�@�@�hs@��@��/@��u@�z�@�9X@��@��;@��
@��w@��@�o@�v�@�V@�ȴ@�33@�+@��\@�~�@�;d@���@��\@�n�@�$�@�V@�^5@�@��h@�hs@��@�bN@���@��
@�dZ@��H@�-@�J@�@��^@��@��@� �@���@��
@���@�S�@��@�^5@�$�@��T@���@��#@���@���@���@�`B@�O�@�?}@�%@�Ĝ@�A�@��w@�33@�33@�@��!@��@�%@��u@�j@��@�bN@��@�K�@�v�@�ff@�^5@���@��@��/@�9X@���@�l�@�"�@�@��y@�v�@�@�O�@�V@���@���@�  @��
@�C�@��R@�E�@��T@��7@�G�@��/@��j@��@�9X@��@���@�S�@���@��R@�5?@���@��h@��j@�  @��@�K�@�o@��H@�$�@��^@��@�O�@�/@���@���@�9X@�  @��w@�|�@�C�@�@�ȴ@��!@�v�@�{@���@�hs@�&�@�%@��`@��@��@��/@��@��u@�j@�1@��@�|�@�dZ@�S�@�33@�
=@�v�@�M�@��7@�O�@�&�@���@��u@�j@�Q�@�1'@�=q@�n�@�@z��@p��@hĜ@`��@V�R@L9X@DZ@<�@6��@01'@*�@$�@�@��@�F@  @�F@ �11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB;dB;dB;dB;dB<jB<jB;dB<jB<jB<jB<jB<jB<jB;dB:^B8RB7LB8RB8RB7LB6FB6FB5?B5?B5?B5?B5?B5?B5?B6FB:^B��B	
=B	,B	B�B	e`B	��B	�B
�B	��B	�
B	�B
&�B
hsB
�B
��B
��B\BB�BYBjB|�B��B�B�jB��B��B��B�sB�B��BBJBVB\B��B��B�fB��BŢB�TB�B�B��B�}B�B�BbNB��B��B�%Bu�By�BcTBK�BE�B=qB9XB'�B\B
��B
ÖB
��B
q�B
Q�B
>wB
/B
�B
+B	��B	�B	�yB	��B	ȴB	��B	ǮB	��B	�}B	�B	��B	�B	o�B	dZB	YB	Q�B	M�B	G�B	@�B	9XB	49B	&�B	"�B	�B	B��B�B�sB�NB�5B��B��B��B�)B�fB�`B�fB�`B�NB�NB�TB�fB�B�B�B��B��B	B	DB	�B	$�B	2-B	;dB	A�B	F�B	YB	r�B	q�B	q�B	o�B	hsB	_;B	[#B	aHB	u�B	~�B	��B	�}B	��B	��B	ĜB	�jB	�}B	ŢB	B	�jB	�-B	��B	��B	��B	��B	�VB	� B	s�B	P�B	7LB	5?B	6FB	:^B	:^B	=qB	E�B	I�B	L�B	J�B	L�B	P�B	R�B	^5B	bNB	aHB	_;B	_;B	^5B	aHB	e`B	hsB	jB	n�B	p�B	q�B	x�B	� B	�B	�+B	�+B	�DB	�VB	�\B	�PB	�PB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	��B	��B	��B	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�!B	�!B	�'B	�FB	�3B	�3B	�3B	�FB	�RB	�RB	�^B	�qB	�wB	�wB	�}B	��B	�}B	�wB	�qB	�wB	�jB	�jB	�jB	�qB	��B	ĜB	ĜB	ĜB	ƨB	ɺB	ǮB	ŢB	ĜB	ƨB	ǮB	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�
B	�B	�
B	�B	�B	�B	�B	�B	�B	�)B	�/B	�5B	�5B	�5B	�;B	�BB	�;B	�HB	�fB	�yB	�B	�yB	�yB	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
%B
+B
1B
1B
1B
1B
1B
	7B
1B
1B
	7B
	7B
	7B
1B
+B
+B
1B

=B
DB
DB

=B
1B

=B
DB
JB
PB
PB
VB
VB
VB
VB
VB
VB
\B
bB
bB
bB
bB
\B
bB
bB
hB
hB
oB
oB
oB
oB
uB
uB
uB
uB
uB
{B
{B
�B
{B
uB
oB
oB
bB
\B
\B
bB
bB
hB
hB
hB
hB
oB
oB
oB
oB
uB
uB
uB
uB
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
#�B
$�B
)�B
2-B
8RB
=qB
C�B
G�B
M�B
Q�B
W
B
\)B
`BB
dZB
gmB
k�B
p�B
t�B
z�B
}�B
�B
�%11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B;9B;<B;<B;9B<?B<@B;9B<?B<>B<@B<?B<?B<=B;;B:4B8(B7"B8+B8*B7 B6B6B5B5B5B5B5B5B5B6B:3B�`B	
B	+�B	BfB	e4B	�mB	��B
uB	��B	��B	�B
&�B
hFB
��B
��B
�UB0BBfBX�BjTB|�B�`B��B�>BΫBͦB��B�GB�B��B �BB&B,B��B��B�7BʔB�pB�#B�rB�MB��B�MB��B��BbB��B��B��Bu�By�Bc!BK�BEnB==B9'B'�B(B
�B
�dB
��B
qvB
Q�B
>DB
.�B
sB
�B	��B	�~B	�DB	��B	�}B	�RB	�wB	͛B	�GB	��B	�VB	��B	ofB	d B	X�B	Q�B	M�B	GwB	@JB	9B	4B	&�B	"�B	NB	�B��B�hB�9B�B��BӽBШBѰB��B�*B�%B�+B�!B�B�B�B�*B�`B�lB�uB��B��B	�B		B	TB	$�B	1�B	;&B	AIB	FjB	X�B	rtB	qlB	qmB	oaB	h6B	^�B	Z�B	a	B	u�B	~�B	��B	�?B	ҶB	ˈB	�_B	�*B	�>B	�cB	�NB	�-B	��B	��B	��B	�gB	�DB	�B	�B	svB	P�B	7B	4�B	6B	: B	:B	=3B	EbB	IzB	L�B	J�B	L�B	P�B	R�B	]�B	bB	aB	^�B	^�B	]�B	aB	e B	h2B	j<B	nWB	pcB	qiB	x�B	�B	��B	��B	��B	�B	�B	�B	�B	�B	�@B	�GB	�MB	�SB	�SB	�tB	�iB	�]B	��B	��B	�jB	�`B	�^B	�lB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�iB	�&B	�DB	�cB	�lB	�PB	�OB	�iB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�B	�B	�B	�B	�-B	�3B	�4B	�9B	�?B	�:B	�2B	�/B	�4B	�'B	�'B	�'B	�/B	�BB	�ZB	�XB	�ZB	�dB	�xB	�mB	�^B	�YB	�eB	�kB	̉B	ΓB	ΗB	ϛB	ТB	ѩB	ҮB	��B	��B	��B	ԼB	ѪB	ТB	ΕB	СB	ҭB	ҮB	үB	ӲB	үB	ҫB	ҬB	ԹB	��B	ԹB	ռB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�"B	�5B	�<B	�8B	�4B	�aB	�nB	�rB	�rB	�xB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	�B
 B

�B
	�B
�B
	�B

�B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
"B
$B
+B
(B
,B
-B
/B
2B
1B
/B
0B
4B
4B
<B
7B
0B
*B
+B
B
B
B
B
B
%B
B
$B
%B
*B
.B
,B
-B
-B
,B
-B
.B
7B
8B
6B
7B
;B
:B
;B
CB
AB
<B
AB
`B
gB
oB
xB
zB
uB
{B
zB
{B
xB
wB
xB
yB
zB
sB
lB
eB
nB
lB
rB
tB
xB
#�B
$�B
)�B
1�B
8B
=.B
CPB
GiB
M�B
Q�B
V�B
[�B
_�B
dB
g%B
k?B
p]B
twB
z�B
}�B
��B
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.53 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             201904051008012019040510080120190405100801  AO  ARCAADJP                                                                    20181121125957    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181121125957  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181121125957  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190405100801  IP                  G�O�G�O�G�O�                