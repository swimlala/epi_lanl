CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-21T12:59:46Z creation      
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
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20181121125946  20190405100754  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  5287                            2C  D   APEX                            6529                            072314                          846 @�ı��r�1   @�Ĳm�N@0��R�d���n�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @�33@�  A   AffA@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B���B���B�  B�  B�  B�  B�  B�  C �C  C�fC  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ�CL�CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt� Dyy�D�3D�@ D�y�D��3D���D�C3D��3D�� D�	�D�I�D���D�� D�  D�6fDچfD�ɚD�fD�C3D�3D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�(�@���Az�A&�GAHz�Ahz�A�=qA�=qA�=qA�=qA�=qA�=qA�=qA�=qB�B
�B�B�B"�B*�B2�B:�BB�BJ�BR�BZ�Bb�Bj�Br�Bz�B�B�B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�B�B�\B��)B��)B�\B�\B�\B�\B�\B�\C �HC��CnC��C��C
��C��C��C��C��C��C��C��C��C��C��C ��C"��C$��C&��C(��C*��C,��C.��C0��C2��C4��C6��C8��C:��C<��C>��C@��CB��CD��CF��CH��CJ�HCL�HCN��CP��CR��CT��CV��CX��CZ��C\��C^��C`��Cb��Cd��Cf��Ch��Cj��Cl��Cn��Cp��Cr��Ct��Cv��Cx��Cz��C|��C~��C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�P�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�7
C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�D !�D ��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D	!�D	��D
!�D
��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D !�D ��D!!�D!��D"!�D"��D#!�D#��D$!�D$��D%!�D%��D&!�D&��D'!�D'��D(!�D(��D)!�D)��D*!�D*��D+!�D+��D,!�D,��D-!�D-��D.!�D.��D/!�D/��D0!�D0��D1!�D1��D2!�D2��D3!�D3��D4!�D4��D5!�D5��D6!�D6��D7!�D7��D8!�D8��D9!�D9��D:!�D:��D;!�D;��D<!�D<��D=!�D=��D>!�D>��D?!�D?��D@!�D@��DA!�DA��DB!�DB��DC!�DC��DD!�DD��DE!�DE��DF!�DF��DG!�DG��DH!�DH��DI!�DI��DJ!�DJ��DK!�DK��DL!�DL��DM!�DM��DN!�DN��DO!�DO��DP!�DP��DQ!�DQ��DR!�DR��DS!�DS��DT!�DT��DU!�DU��DV!�DV��DW!�DW��DX!�DX��DY!�DY��DZ!�DZ��D[!�D[��D\!�D\��D]!�D]��D^!�D^��D_!�D_��D`!�D`��Da!�Da��Db!�Db��Dc!�Dc��Dd!�Dd��De!�De��Df!�Df��Dg!�Dg��Dh!�Dh��Di!�Di��Dj!�Dj��Dk!�Dk��Dl!�Dl��Dm!�Dm��Dn!�Dn��Do!�Do��Dp!�Dp��Dq!�Dq��Dr!�Dr��Ds!�Ds��Dt!�Dt��Dt��Dy��D�$)D�P�D���D��)D��D�T)D��)D���D��D�Z�D���D���D��D�G\Dڗ\D�ڐD�\D�T)D�)D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A��A��A���A���A���A���A���A��A���A���A�  A�A�A���A��A��A���A�A�?}A�bNA��;A��HAݧ�A�hsAܸRA�v�AٓuA�E�A�bA׸RA��A�ffA���A��/AփA�33A��
A� �A��/A�I�A�x�A�-Aβ-A��HA�v�A���A�VA�A�A�K�Aɴ9Aȧ�A�  A�E�A�E�Aĕ�A�A�AÏ\A�A��hA���A��\A�ȴA�p�A�-A�&�A�1A���A��A�&�A��7A�r�A�l�A���A���A�1'A�ȴA���A�?}A�bNA��A�(�A�E�A��#A�ffA���A�(�A�bNA���A�VA�p�A���A� �A���A���A�%A�n�A�oA�+A�\)A���A���A�I�A��uA�p�A{�;Aw��Au��At��As�FAq��ApI�An�Ak�hAhjAe��Ac�AaA]�
A\  AZ��AW�ASx�AQ��AO�AN9XAL�AJ�AG��AB�/A?&�A>�DA>bA=;dA< �A9;dA6ĜA4��A3p�A29XA/XA-�A+��A*5?A*�A)�PA(��A(jA&��A&A$�+A#;dA �A�
Ap�A��A��A�A��AƨAC�A�uAJA+A�A�AG�AĜAz�A�#A��A1AK�A(�A�+A�mA"�AbNA�;A�PA
=A
  A	��A	hsAAS�A7LA��AjA;dA~�A��AZAp�A;dA�A%A�A
=AVA�A/A7LA&�A ĜA  �@���@���@�z�@��@���@��@��@�E�@���@�/@��j@�-@��#@�hs@�I�@���@�bN@�j@��F@���@��^@�K�@�p�@��@�o@�\)@�M�@�?}@�@�O�@�/@���@�R@�x�@�@�r�@�F@�w@��H@�
=@�I�@�j@�Q�@蛦@�?}@�I�@�dZ@�
=@�5?@���@��@�@�h@�;d@�G�@��D@��@��u@���@���@���@��@���@���@��@��u@�A�@�ƨ@�;d@ޗ�@���@�`B@���@ܣ�@�Q�@���@ۍP@�K�@�@�v�@�E�@�@���@���@�hs@��@أ�@��;@�S�@��@��@�"�@�+@�@ָR@���@���@�/@���@�I�@��@���@�t�@�\)@��y@�^5@�{@���@�?}@Ѓ@�bN@�Q�@�b@ϕ�@�;d@�+@�^5@�?}@��/@̬@̃@�I�@���@ˮ@˅@��@���@���@���@� �@��@�dZ@��y@ƸR@�n�@�{@���@�/@���@���@��/@��`@�hs@�Z@ă@��m@�t�@�;d@�@���@��@�7L@�z�@�b@��m@��
@��@��P@��@�ff@��^@�7L@��`@���@���@�Ĝ@��j@��D@��F@�V@��^@�`B@��@��@�(�@��w@�dZ@��!@�E�@�-@��@�X@��/@�bN@���@�t�@�@��H@�^5@�J@��@��#@��@�hs@�`B@�X@�G�@�O�@�`B@�p�@��@���@���@�&�@�z�@���@��P@�S�@�ȴ@�n�@�@��7@�/@���@��@��9@��@� �@��@���@�;d@��@�^5@���@��h@�X@��@���@�Ĝ@��@���@��@�j@�1'@���@��F@�t�@�33@���@���@�n�@�J@��h@�`B@��@���@�  @��F@�\)@�+@��y@��+@���@���@���@�j@�1@��m@��m@��;@��
@��w@�|�@�C�@���@���@�n�@�-@��@���@��^@���@�X@�/@�V@��/@��@��@��@�\)@��!@�n�@�Z@��@�ff@��u@��@{�
@r~�@jJ@`��@W+@L�@D9X@<�@6$�@0Ĝ@(  @!hs@�j@1'@�m@+@�m111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A��A��A��A���A���A���A���A���A��A���A���A�  A�A�A���A��A��A���A�A�?}A�bNA��;A��HAݧ�A�hsAܸRA�v�AٓuA�E�A�bA׸RA��A�ffA���A��/AփA�33A��
A� �A��/A�I�A�x�A�-Aβ-A��HA�v�A���A�VA�A�A�K�Aɴ9Aȧ�A�  A�E�A�E�Aĕ�A�A�AÏ\A�A��hA���A��\A�ȴA�p�A�-A�&�A�1A���A��A�&�A��7A�r�A�l�A���A���A�1'A�ȴA���A�?}A�bNA��A�(�A�E�A��#A�ffA���A�(�A�bNA���A�VA�p�A���A� �A���A���A�%A�n�A�oA�+A�\)A���A���A�I�A��uA�p�A{�;Aw��Au��At��As�FAq��ApI�An�Ak�hAhjAe��Ac�AaA]�
A\  AZ��AW�ASx�AQ��AO�AN9XAL�AJ�AG��AB�/A?&�A>�DA>bA=;dA< �A9;dA6ĜA4��A3p�A29XA/XA-�A+��A*5?A*�A)�PA(��A(jA&��A&A$�+A#;dA �A�
Ap�A��A��A�A��AƨAC�A�uAJA+A�A�AG�AĜAz�A�#A��A1AK�A(�A�+A�mA"�AbNA�;A�PA
=A
  A	��A	hsAAS�A7LA��AjA;dA~�A��AZAp�A;dA�A%A�A
=AVA�A/A7LA&�A ĜA  �@���@���@�z�@��@���@��@��@�E�@���@�/@��j@�-@��#@�hs@�I�@���@�bN@�j@��F@���@��^@�K�@�p�@��@�o@�\)@�M�@�?}@�@�O�@�/@���@�R@�x�@�@�r�@�F@�w@��H@�
=@�I�@�j@�Q�@蛦@�?}@�I�@�dZ@�
=@�5?@���@��@�@�h@�;d@�G�@��D@��@��u@���@���@���@��@���@���@��@��u@�A�@�ƨ@�;d@ޗ�@���@�`B@���@ܣ�@�Q�@���@ۍP@�K�@�@�v�@�E�@�@���@���@�hs@��@أ�@��;@�S�@��@��@�"�@�+@�@ָR@���@���@�/@���@�I�@��@���@�t�@�\)@��y@�^5@�{@���@�?}@Ѓ@�bN@�Q�@�b@ϕ�@�;d@�+@�^5@�?}@��/@̬@̃@�I�@���@ˮ@˅@��@���@���@���@� �@��@�dZ@��y@ƸR@�n�@�{@���@�/@���@���@��/@��`@�hs@�Z@ă@��m@�t�@�;d@�@���@��@�7L@�z�@�b@��m@��
@��@��P@��@�ff@��^@�7L@��`@���@���@�Ĝ@��j@��D@��F@�V@��^@�`B@��@��@�(�@��w@�dZ@��!@�E�@�-@��@�X@��/@�bN@���@�t�@�@��H@�^5@�J@��@��#@��@�hs@�`B@�X@�G�@�O�@�`B@�p�@��@���@���@�&�@�z�@���@��P@�S�@�ȴ@�n�@�@��7@�/@���@��@��9@��@� �@��@���@�;d@��@�^5@���@��h@�X@��@���@�Ĝ@��@���@��@�j@�1'@���@��F@�t�@�33@���@���@�n�@�J@��h@�`B@��@���@�  @��F@�\)@�+@��y@��+@���@���@���@�j@�1@��m@��m@��;@��
@��w@�|�@�C�@���@���@�n�@�-@��@���@��^@���@�X@�/@�V@��/@��@��@��@�\)@��!@�n�G�O�@��@�ff@��u@��@{�
@r~�@jJ@`��@W+@L�@D9X@<�@6$�@0Ĝ@(  @!hs@�j@1'@�m@+@�m111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	VB	\B	\B	bB	bB	\B	\B	VB	PB	hB	�B	�B	�B	�B	%�B	5?B	E�B	l�B	�%B	�FB
B
�B
+B
-B
A�B
K�B
"�B	�B	ŢB	B	��B
1B
  B
	7B
8RB
bNB
cTB
m�B
u�B
w�B
q�B
y�B
� B
��B
�B
�FB
ĜB
�B
��B�B$�B>wBe`Br�B~�B�oBB�fB�ZB�`B�B2-B�VB��B�jB�B�B�B��BŢB�-Bx�B2-B�B��B��BPB�B49BS�BQ�B�B��B�BƨB�B��B��B�BiyBZBG�B�BB
��B
�B
�yB
�
B
�wB
�-B
��B
u�B
_;B
E�B
�B
B	�B	�TB	�)B	��B	��B	ƨB	�qB	�B	��B	�DB	|�B	r�B	bNB	YB	R�B	F�B	9XB	2-B	(�B	 �B	�B	bB	+B��B�B�B�B�sB�`B�/B�#B�
B�B��B��B��BǮBÖBB��B��BƨBŢBǮBŢB�}B�jB�dB�jB�qB�wBĜBɺB��B��B��B�
B�)B�/B�HB�`B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B	B	PB	\B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	&�B	9XB	K�B	P�B	R�B	YB	_;B	bNB	cTB	hsB	iyB	jB	hsB	gmB	ffB	ffB	dZB	bNB	cTB	dZB	hsB	y�B	~�B	}�B	~�B	�B	�JB	�bB	�bB	�PB	�JB	�B	�B	}�B	{�B	�B	~�B	}�B	�%B	�1B	�DB	�B	�B	}�B	�B	�%B	�7B	�VB	�PB	��B	��B	�B	�B	�RB	��B	�qB	�jB	��B	�wB	�qB	�wB	��B	�qB	�LB	�^B	�jB	�jB	B	ŢB	ŢB	ƨB	ƨB	ƨB	ƨB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�
B	�
B	�
B	�
B	�
B	�B	�#B	�5B	�HB	�ZB	�NB	�NB	�`B	�fB	�fB	�fB	�fB	�mB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�sB	�sB	�mB	�mB	�mB	�`B	�HB	�/B	�;B	�/B	�#B	�#B	�B	�B	�B	�B	�B	�#B	�/B	�HB	�mB	�fB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
+B
+B
+B
+B
+B
+B
+B
+B
+B
+B
+B
+B
+B
+B
+B
+B
+B
1B
1B
1B
1B
1B
	7B
	7B
	7B
	7B
1B
1B
1B
1B
+B
	7B
1B
	7B
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

=B

=B
DB
DB
DB
DB
JB
PB
VB
\B
\B
bB
bB
bB
bB
bB
hB
bB
bB
oB
oB
hB
K�B
VB
uB
�B
!�B
.B
49B
>wB
D�B
G�B
M�B
R�B
YB
^5B
cTB
ffB
jB
o�B
t�B
w�B
{�B
~�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B	0B	4B	5B	8B	8B	5B	3B	0B	'B	@B	XB	[B	lB	�B	%�B	5B	E{B	lcB	��B	�B
�B
�B
*�B
,�B
AaB
K�B
"�B	�]B	�yB	�gB	ʗB
B	��B
	B
8(B
b B
c*B
mhB
u�B
w�B
q�B
y�B
�B
�{B
��B
�B
�rB
�aB
��B[B$�B>NBe3Br�B~�B�BB�bB�;B�/B�4B��B2B�*B��B�:B��B�zB�lB˙B�sB��Bx�B2BYB��B��BBiB4BS�BQ�B�B��B�MB�uB��B��B�\B��BiFBY�BGzByB�B
��B
�B
�DB
��B
�DB
��B
�zB
u�B
_B
EoB
�B
�B	�bB	�B	��B	��B	ˑB	�tB	�8B	��B	�iB	�B	|�B	ryB	bB	X�B	R�B	FpB	9 B	1�B	(�B	 �B	aB	'B	�B��B�iB�WB�DB�9B�%B��B��B��B��BҵB��BϡB�pB�XB�OB�EB�IB�jB�fB�rB�fB�@B�*B�$B�.B�2B�8B�]B�|BˇBΛBӷB��B��B��B�B� B�?B�?B�LB�YB�lB�vB��B��B�zB�|B��B��B��B��B��B��B��B	�B	B	B	GB	[B	fB	jB	VB	^B	pB	|B	!�B	&�B	9B	K�B	P�B	R�B	X�B	^�B	bB	cB	h1B	i6B	j<B	h2B	g+B	f$B	f$B	dB	b
B	cB	dB	h4B	y�B	~�B	}�B	~�B	��B	�B	� B	�"B	�B	�B	��B	��B	}�B	{�B	��B	~�B	}�B	��B	��B	�B	��B	��B	}�B	��B	��B	��B	�B	�B	�FB	��B	��B	��B	�B	�HB	�/B	�'B	�?B	�7B	�/B	�5B	�AB	�/B	�	B	�B	�$B	�'B	�JB	�`B	�`B	�dB	�gB	�dB	�dB	�lB	˄B	͐B	ϚB	ҲB	ѪB	ӵB	ҮB	ҰB	ҭB	ӵB	ӵB	ԻB	ԹB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�#B	�$B	�%B	�!B	�+B	�0B	�2B	�6B	�7B	�BB	�HB	�HB	�FB	�DB	�CB	�AB	�HB	�CB	�DB	�AB	�:B	�>B	�?B	�7B	�0B	�.B	�*B	�(B	�)B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�)B	�"B	�AB	�CB	�?B	�CB	�AB	�UB	�fB	�uB	�oB	�vB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
	�B
	�B
B

�B
B
B
B
B
B
B
B
B
B
B
B
B
%B
B
B
*B
*B
#G�O�B
B
/B
VB
!�B
-�B
3�B
>0B
DSB
GiB
M�B
R�B
X�B
]�B
cB
f B
j:B
oYB
txB
w�B
{�B
~�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.53 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             201904051007542019040510075420190405100754  AO  ARCAADJP                                                                    20181121125946    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181121125946  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181121125946  QCF$                G�O�G�O�G�O�4000            UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190405100754  IP                  G�O�G�O�G�O�                