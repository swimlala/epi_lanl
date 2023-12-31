CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-21T12:59:45Z creation      
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
_FillValue                    �,Argo profile    3.1 1.2 19500101000000  20181121125945  20190405100753  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  5287                            2C  D   APEX                            6529                            072314                          846 @��Ӡt}1   @��ffm�@0�7KƧ��d�~��"�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   BffBffBffB   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�ffB뙚B���B�  B�  B�  C   C  C�C�C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj�Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.�fD/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dyl�D��D�@ D���D�� D��3D�@ D���D��3D�  D�0 D��fD��3D�fD�C3Dړ3D��3D�� D�9�D�vf11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�\)@���Az�A(z�AHz�Ahz�A�=qA�=qA�=qA�=qA�=qA�=qA�=qA�=qB�B
�B�B�B"�B*�B2�B:�BB�BJ�BR�BZ�Bb�Bj�Br�Bz�B�\B�\B�B�B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�B�B�B�B�\B�\B�\B�\B�\B�\B�\B�\B�\B�u�B��B��)B�\B�\B�\C ��C��C�HC�HC��C
��C��C��C��C��C��C��C��C��C��C��C ��C"��C$��C&��C(��C*��C,��C.��C0��C2��C4��C6��C8��C:��C<��C>��C@��CB��CD��CF��CH��CJ��CL��CN��CP��CR��CT��CV��CX��CZ��C\��C^��C`��Cb��Cd��Cf��Ch��Cj�HCl��Cn��Cp��Cr��Ct��Cv��Cx��Cz��C|��C~��C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�D !�D ��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D	!�D	��D
!�D
��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D !�D ��D!!�D!��D"!�D"��D#!�D#��D$!�D$��D%!�D%��D&!�D&��D'!�D'��D(!�D(��D)!�D)��D*!�D*��D+!�D+��D,!�D,��D-!�D-��D.!�D.�RD/!�D/��D0!�D0��D1!�D1��D2!�D2��D3!�D3��D4!�D4��D5!�D5��D6!�D6��D7!�D7��D8!�D8��D9!�D9��D:!�D:��D;!�D;��D<!�D<��D=!�D=��D>!�D>��D?!�D?��D@!�D@��DA!�DA��DB!�DB��DC!�DC��DD!�DD��DE!�DE��DF!�DF��DG!�DG��DH!�DH��DI!�DI��DJ!�DJ��DK!�DK��DL!�DL��DM!�DM��DN!�DN��DO!�DO��DP!�DP��DQ!�DQ��DR!�DR��DS!�DS��DT!�DT��DU!�DU��DV!�DV��DW!�DW��DX!�DX��DY!�DY��DZ!�DZ��D[!�D[��D\!�D\��D]!�D]��D^!�D^��D_!�D_��D`!�D`��Da!�Da��Db!�Db��Dc!�Dc��Dd!�Dd��De!�De��Df!�Df��Dg!�Dg��Dh!�Dh��Di!�Di��Dj!�Dj��Dk!�Dk��Dl!�Dl��Dm!�Dm��Dn!�Dn��Do!�Do��Dp!�Dp��Dq!�Dq��Dr!�Dr��Ds!�Ds��Dt!�Dt��Dt�Dy��D�-�D�P�D���D���D��)D�P�D���D��)D��D�@�D��\D��)D�\D�T)Dڤ)D��)D� �D�J�D�\11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�A�A�A߼jA�ĜA�ĜA�ĜA�ȴA߲-Aߟ�Aߙ�A߲-A�t�A�K�A�A�A�C�A�&�A���A��A��yA��/A���Aޝ�A�n�A�bNA�I�A�1Aݣ�A�`BAܙ�A��A�
=A��HA���A��A�bNAӝ�A��A�$�A��A�v�A�VA��A҉7A���A�XAЏ\A��A�+A�t�A�I�ȂhA�z�A���A�;dA��
A��A�JAƶFAžwAăA���A���A�l�A��9A�JA�ffA��HA��A���A�t�A�^5A���A�O�A�{A��A�1A��jA�K�A�C�A�VA��DA���A���A���A���A��A���A��/A�\)A��A��A�M�A���A�bA�A�ZA��hA���A�jA��-A�C�A��jA�?}A�Q�A��A��A��A�A��;A�dZA��uA�E�A~��A}�wA|��A|{Ax �Av�At�`Ar~�Ap-Ao�Aln�Ah^5Ag�Af�AdA`��A^I�A\bAZE�AWƨAU33AS��AN��AK�AK+AI�mAE�;AA;dAA
=A?/A>�\A=�FA<�HA<�+A;��A:��A:VA:{A9x�A7p�A4�+A3?}A2^5A1��A0�`A0M�A/�A/�FA/�A.M�A,�A+��A+XA*�`A*E�A)��A(�+A(1A'�wA'��A&��A&��A&^5A%��A%"�A$�9A#�#A"�9A!�A ��A bA A�A -AC�A�PA�A&�A�7An�A��A�DA�9A�-A�A��A��A��A��A�AA
��A
9XA��A��AA�A�A�A�/A=qA��A�`AE�A��A�7A;dA��AĜA�9A�uAA�A�A�PAA v�A {@��P@�^5@�7L@��9@���@�r�@�Q�@�1@�|�@�o@���@�ȴ@�@��j@�I�@�b@��F@�|�@�@�M�@��^@�I�@��
@�S�@�~�@�p�@�@���@���@�K�@�P@�K�@�
=@�!@�ff@��T@�1'@�w@�@�
=@�ff@�hs@�`B@�O�@�O�@�?}@�&�@��@��/@�D@��
@�o@���@�V@�%@���@���@�~�@��T@�Ĝ@�j@�@�bN@�(�@�1@߾w@�;d@�v�@��#@ݡ�@�x�@�X@�?}@���@�z�@�1@ۥ�@��@ڗ�@�M�@���@ٲ-@�?}@���@ج@�Z@��@֗�@�{@���@Չ7@�hs@���@�ƨ@�t�@�+@�n�@���@ѩ�@�p�@�&�@�Ĝ@�r�@�I�@Ͼw@ύP@�\)@�"�@���@Ώ\@�^5@���@�X@̓u@�b@˝�@�S�@�"�@��@ʸR@�ff@��@�%@ț�@�z�@�Q�@�t�@�n�@�{@�7L@���@�z�@�Z@��
@Ý�@�K�@�@��@�@�ȴ@°!@�E�@��^@�%@�Z@�b@��m@��
@���@�33@�~�@�V@�E�@��-@�X@��@�Q�@��;@�|�@��@�ff@�J@��@��^@��7@�p�@��@��D@�(�@��
@�ƨ@��@��@�l�@�C�@�@��!@��+@�E�@���@�`B@�X@���@��@���@�\)@�"�@�@���@��\@���@���@���@�j@�A�@�Z@���@���@�j@�1@���@�"�@��!@�J@���@��@���@���@���@��h@��@�O�@��j@��u@�Q�@��@�S�@�"�@�o@��H@��+@�J@��@�@��@��@��`@��`@���@�Q�@�1@��F@�dZ@�v�@��#@���@�G�@�j@���@��P@�o@���@���@�^5@��@��@�X@�?}@�%@���@���@�Q�@�9X@�A�@��@��P@�o@�ȴ@��+@�M�@�$�@���@�K�@��T@�+@��u@v{@kdZ@a��@Z=q@Q�7@I�#@@�`@9�@1�7@)hs@%�T@��@�\@p�@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111   A�A�A�A߼jA�ĜA�ĜA�ĜA�ȴA߲-Aߟ�Aߙ�A߲-A�t�A�K�A�A�A�C�A�&�A���A��A��yA��/A���Aޝ�A�n�A�bNA�I�A�1Aݣ�A�`BAܙ�A��A�
=A��HA���A��A�bNAӝ�A��A�$�A��A�v�A�VA��A҉7A���A�XAЏ\A��A�+A�t�A�I�ȂhA�z�A���A�;dA��
A��A�JAƶFAžwAăA���A���A�l�A��9A�JA�ffA��HA��A���A�t�A�^5A���A�O�A�{A��A�1A��jA�K�A�C�A�VA��DA���A���A���A���A��A���A��/A�\)A��A��A�M�A���A�bA�A�ZA��hA���A�jA��-A�C�A��jA�?}A�Q�A��A��A��A�A��;A�dZA��uA�E�A~��A}�wA|��A|{Ax �Av�At�`Ar~�Ap-Ao�Aln�Ah^5Ag�Af�AdA`��A^I�A\bAZE�AWƨAU33AS��AN��AK�AK+AI�mAE�;AA;dAA
=A?/A>�\A=�FA<�HA<�+A;��A:��A:VA:{A9x�A7p�A4�+A3?}A2^5A1��A0�`A0M�A/�A/�FA/�A.M�A,�A+��A+XA*�`A*E�A)��A(�+A(1A'�wA'��A&��A&��A&^5A%��A%"�A$�9A#�#A"�9A!�A ��A bA A�A -AC�A�PA�A&�A�7An�A��A�DA�9A�-A�A��A��A��A��A�AA
��A
9XA��A��AA�A�A�A�/A=qA��A�`AE�A��A�7A;dA��AĜA�9A�uAA�A�A�PAA v�A {@��P@�^5@�7L@��9@���@�r�@�Q�@�1@�|�@�o@���@�ȴ@�@��j@�I�@�b@��F@�|�@�@�M�@��^@�I�@��
@�S�@�~�@�p�@�@���@���@�K�@�P@�K�@�
=@�!@�ff@��T@�1'@�w@�@�
=@�ff@�hs@�`B@�O�@�O�@�?}@�&�@��@��/@�D@��
@�o@���@�V@�%@���@���@�~�@��T@�Ĝ@�j@�@�bN@�(�@�1@߾w@�;d@�v�@��#@ݡ�@�x�@�X@�?}@���@�z�@�1@ۥ�@��@ڗ�@�M�@���@ٲ-@�?}@���@ج@�Z@��@֗�@�{@���@Չ7@�hs@���@�ƨ@�t�@�+@�n�@���@ѩ�@�p�@�&�@�Ĝ@�r�@�I�@Ͼw@ύP@�\)@�"�@���@Ώ\@�^5@���@�X@̓u@�b@˝�@�S�@�"�@��@ʸR@�ff@��@�%@ț�@�z�@�Q�@�t�@�n�@�{@�7L@���@�z�@�Z@��
@Ý�@�K�@�@��@�@�ȴ@°!@�E�@��^@�%@�Z@�b@��m@��
@���@�33@�~�@�V@�E�@��-@�X@��@�Q�@��;@�|�@��@�ff@�J@��@��^@��7@�p�@��@��D@�(�@��
@�ƨ@��@��@�l�@�C�@�@��!@��+@�E�@���@�`B@�X@���@��@���@�\)@�"�@�@���@��\@���@���@���@�j@�A�@�Z@���@���@�j@�1@���@�"�@��!@�J@���@��@���@���@���@��h@��@�O�@��j@��u@�Q�@��@�S�@�"�@�o@��H@��+@�J@��@�@��@��@��`@��`@���@�Q�@�1@��F@�dZ@�v�@��#@���@�G�@�j@���@��P@�o@���@���@�^5@��@��@�X@�?}@�%@���@���@�Q�@�9X@�A�@��@��P@�o@�ȴ@��+@�M�G�O�@���@�K�@��T@�+@��u@v{@kdZ@a��@Z=q@Q�7@I�#@@�`@9�@1�7@)hs@%�T@��@�\@p�@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
XB
XB
XB
XB
XB
XB
XB
XB
W
B
W
B
VB
W
B
S�B
Q�B
Q�B
Q�B
P�B
N�B
N�B
M�B
L�B
K�B
G�B
D�B
C�B
@�B
;dB
2-B
+B
�B
�B
=qB
.B
uB	��B	�B
�B
=qB
u�B
|�B
s�B
p�B
q�B
�%B
��B
��B
��B
�B
�dB
��B
��B
�5B
�sB
�B+BJB
��B
�mB�B�=B��B�3BÖB�sBuB�B6FBVBn�By�Bz�Bz�Bt�Bv�B�7B�VB��B��B�!B�9B�jB�qB�3B��B��B�VB�DB�%B{�BjB_;B5?B"�B!�BB�/B�LB�uB�+Bt�BjBW
BF�B'�B�BB
��B
�'B
��B
�VB
�B
m�B
K�B
<jB
5?B
.B
'�B
PB	��B	��B	�ZB	��B	��B	�LB	��B	��B	�\B	�B	jB	[#B	M�B	B�B	5?B	'�B	�B	uB	�B	�B	oB	B��B��B��B�B�B�B�B�B�B�B�yB�mB�fB�B�B�B�B��B��B��B	B	B	B	PB	oB	{B	�B	�B	�B	+B	/B	1'B	9XB	;dB	?}B	@�B	=qB	;dB	9XB	6FB	0!B	-B	)�B	.B	49B	8RB	5?B	&�B	�B	�B	JB	B��B��B��B��B��B��B	  B��B	B	bB	oB	oB	{B	�B	�B	�B	 �B	'�B	,B	,B	.B	.B	1'B	5?B	8RB	<jB	>wB	?}B	@�B	D�B	E�B	G�B	P�B	VB	[#B	_;B	aHB	ffB	l�B	n�B	o�B	p�B	s�B	{�B	� B	�B	�%B	�1B	�\B	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�?B	�FB	�LB	�RB	�qB	��B	��B	B	��B	�wB	��B	B	ÖB	ƨB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�#B	�/B	�;B	�BB	�HB	�NB	�TB	�ZB	�ZB	�ZB	�ZB	�TB	�ZB	�ZB	�`B	�`B	�fB	�mB	�mB	�sB	�sB	�sB	�sB	�yB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B	��B
  B
  B	��B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
  B
  B
B
B
B
B
B
B
  B	��B	��B	��B
  B
B
B
%B
%B
%B
%B
%B
B
B
%B
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
%B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
+B
+B
%B
%B
B
B
B
B
%B
%B
+B
+B
1B
1B
	7B
	7B
	7B
	7B
	7B

=B
JB
PB
PB
\B
\B
\B
\B
\B
uB
oB
bB
�B
�B
&�B
.B
<jB
?}B
E�B
K�B
O�B
S�B
W
B
_;B
dZB
ffB
k�B
r�B
u�B
w�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111   B
W�B
W�B
W�B
W�B
W�B
W�B
W�B
W�B
V�B
V�B
U�B
V�B
S�B
Q�B
Q�B
Q�B
P�B
N�B
N�B
M�B
L�B
K�B
G�B
DtB
CoB
@XB
;<B
2B
*�B
�B
mB
=HB
-�B
KB	��B	�cB
}B
=JB
u�B
|�B
s�B
p|B
q�B
��B
�xB
��B
��B
��B
�9B
ίB
��B
�B
�JB
�B�BB
��B
�ABlB�B��B�B�hB�IBIB�B6BU�BnkBy�Bz�Bz�Bt�Bv�B�	B�%B�gB��B��B�
B�>B�CB�B��B�[B�&B�B��B{�BjOB_B5B"�B!�B�B��B�B�DB��Bt�BjKBV�BFtB'�BSB�B
ҿB
��B
�~B
� B
��B
mZB
K�B
<4B
5
B
-�B
'�B
B	��B	�B	�$B	һB	̗B	�B	��B	�bB	�%B	��B	jFB	Z�B	M�B	BUB	5B	'�B	tB	=B	`B	XB	6B	 �B�B��B��B�pB�]B�UB�MB�JB�CB�EB�>B�2B�)B�CB�PB�fB�zB��B��B��B	�B	�B	�B	B	3B	?B	WB	dB	�B	*�B	.�B	0�B	9B	;'B	??B	@FB	=2B	;&B	9B	6B	/�B	,�B	)�B	-�B	3�B	8B	5B	&�B	yB	PB	B	�B��B��B��B��B��B��B��B��B	�B	"B	/B	0B	=B	lB	rB	jB	 �B	'�B	+�B	+�B	-�B	-�B	0�B	4�B	8B	<+B	>8B	?<B	@CB	DZB	E_B	GnB	P�B	U�B	Z�B	^�B	aB	f%B	lLB	nWB	o\B	pcB	suB	{�B	�B	��B	��B	��B	�B	�2B	�8B	�?B	�>B	�FB	�EB	�QB	�cB	�kB	�dB	�^B	�[B	�QB	�kB	�qB	�pB	��B	��B	��B	��B	�B	�
B	�B	�/B	�@B	�IB	�MB	�GB	�5B	�@B	�NB	�TB	�eB	�yB	�yB	ˆB	̋B	̋B	͑B	ΙB	ΘB	ХB	УB	ԼB	��B	��B	��B	��B	��B	��B	��B	� B	�B	�	B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�+B	�,B	�0B	�2B	�/B	�0B	�5B	�0B	�/B	�6B	�8B	�;B	�<B	�>B	�<B	�BB	�CB	�DB	�DB	�AB	�CB	�AB	�AB	�BB	�AB	�DB	�HB	�HB	�HB	�HB	�GB	�PB	�PB	�OB	�NB	�VB	�ZB	�ZB	�cB	�dB	�`B	�`B	�`B	�`B	�gB	�hB	�gB	�aB	�fB	�nB	�gB	�kB	�mB	�jB	�rB	�xB	�yB	��B	�~B	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
 �B
 �B
 �B
�B
 �B
 �B	��B	��B
 �B
 �B
 �B
 �B
 �B
 �B	��B	��B	��B	��B	��B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	�B
B
B
B
B
B
B
B
G�O�B
-B
B
NB
pB
&�B
-�B
<%B
?8B
E`B
K�B
O�B
S�B
V�B
^�B
dB
f!B
kAB
rkB
uB
w�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.53 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             201904051007532019040510075320190405100753  AO  ARCAADJP                                                                    20181121125945    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181121125945  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181121125945  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190405100753  IP                  G�O�G�O�G�O�                