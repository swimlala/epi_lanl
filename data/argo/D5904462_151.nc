CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-21T12:59:48Z creation      
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
_FillValue                    �,Argo profile    3.1 1.2 19500101000000  20181121125948  20190405100756  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  5287                            2C  D   APEX                            6529                            072314                          846 @��2]|߰1   @��2��|@0e`A�7L�dk"��`B1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   A   B   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BHffBP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C�C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl�Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DB��DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  DtL�Dy� D�	�D�I�D���D��fD�  D�P D���D�� D�fD�C3D�vfD�� D�	�D�P Dڃ3D�ɚD�fD�6fD�D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�(�@���Az�A(z�AHz�Ahz�A�=qA�=qA�=qA�=qA�=qA�=qA�=qA�=qB�B
�B�B�B"�B*�B2�B:�BB�BJ�BR�BZ�Bb�Bj�Br�Bz�B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�B�B�B�B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\C ��C��C��C��C��C
��C��C��C��C��C��C��C��C��C��C�HC ��C"��C$��C&��C(��C*��C,��C.��C0��C2��C4��C6��C8��C:��C<��C>��C@��CB��CD��CF��CH��CJ��CL��CN��CP��CR��CT��CV��CX��CZ��C\��C^��C`��Cb��Cd��Cf��Ch��Cj��Cl�HCn��Cp��Cr��Ct��Cv��Cx��Cz��C|��C~��C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�P�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�D !�D ��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D	!�D	��D
!�D
��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D !�D ��D!!�D!��D"!�D"��D#!�D#��D$!�D$��D%!�D%��D&!�D&��D'!�D'��D(!�D(��D)!�D)��D*!�D*��D+!�D+��D,!�D,��D-!�D-��D.!�D.��D/!�D/��D0!�D0��D1!�D1��D2!�D2��D3!�D3��D4!�D4��D5!�D5��D6!�D6��D7!�D7��D8!�D8��D9!�D9��D:!�D:��D;!�D;��D<!�D<��D=!�D=��D>!�D>��D?!�D?��D@!�D@��DA!�DA��DB!�DB��DC�DC��DD!�DD��DE!�DE��DF!�DF��DG!�DG��DH!�DH��DI!�DI��DJ!�DJ��DK!�DK��DL!�DL��DM!�DM��DN!�DN��DO!�DO��DP!�DP��DQ!�DQ��DR!�DR��DS!�DS��DT!�DT��DU!�DU��DV!�DV��DW!�DW��DX!�DX��DY!�DY��DZ!�DZ��D[!�D[��D\!�D\��D]!�D]��D^!�D^��D_!�D_��D`!�D`��Da!�Da��Db!�Db��Dc!�Dc��Dd!�Dd��De!�De��Df!�Df��Dg!�Dg��Dh!�Dh��Di!�Di��Dj!�Dj��Dk!�Dk��Dl!�Dl��Dm!�Dm��Dn!�Dn��Do!�Do��Dp!�Dp��Dq!�Dq��Dr!�Dr��Ds!�Ds��Dt!�Dtn�Dy��D��D�Z�D���D��\D�0�D�`�D���D���D�'\D�T)D��\D���D��D�`�Dڔ)D�ڐD�\D�G\D�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A��A��A��A��A��A��A��A��A�"�A�&�A�(�A�+A�-A�/A�/A�1'A�1'A�5?A�7LA�9XA�;dA�E�A�M�A�I�A�G�A�C�A�-A��A߾wAߛ�A�33A���A��`A޶FAދDA�$�A�E�A�bNA�Q�Aڣ�A�1'A��
A�9XAذ!A���A���A��Aө�A�ȴA�
=AύPA��AͼjA�A�bA�ƨA�n�AʼjAɴ9AȬAǴ9AƉ7A��TAŝ�A�E�A��yA��mA�p�A�A�r�A�I�A�ffA�n�A�bA�"�A�K�A��A�jA���A� �A�n�A��A�x�A���A�
=A�%A�{A�\)A�ȴA�E�A���A���A���A���A�ZA�x�A���A��RA��A��TA��uA���A�hsA��PA��A���A�33A�9XA���A���A�;dA�M�A���A�`BA���A}�Ax��Au;dAo��Aj5?Ae�Ad��Ac��Ab�Aa��A^�9AX�AT��APffAKK�AIƨAG��AD1'A@�DA<��A<A�A8��A5��A3&�A1�hA0z�A.-A,jA+��A)�wA(ZA&�HA&M�A%S�A%�A#|�A#�A#��A#+A"�yA"�RA"�DA"��A"�DA ��A�wA�7AȴAM�A�A��A�yA�A-A�Ap�Al�A�AdZA?}A��AS�A=qA��A�A�AdZA�#A�A=qAƨAr�A"�A+A�`A�yA�+A�A�hA+A��A�jAv�AA��Al�AVA	&�Ar�A�mAI�A�A�AZAJA��A��A�yAbNAVAt�A�PAt�Az�A��A��A�Ar�A�A��AS�A%A ��A $�@�+@�&�@��y@���@��-@�Ĝ@��@�"�@�-@�O�@�j@�b@���@�C�@��@�J@��@�G�@�&�@�I�@�K�@�n�@�G�@��@�%@�Ĝ@�u@�@�7@��`@�  @�K�@�v�@�$�@�{@���@���@�9@�I�@�dZ@�$�@�`B@�1'@��y@��@�X@ܴ9@�1'@� �@�  @۾w@ۍP@�t�@��H@�@��/@�1'@ם�@�l�@�K�@��@�o@���@���@և+@�ff@�V@��@�7L@�z�@� �@��;@�A�@�(�@Ӆ@�\)@�ff@�@�%@�1'@�+@Η�@�V@���@Ͳ-@͡�@͑h@�p�@�%@�z�@��
@�|�@�o@ʏ\@�5?@��#@�`B@ț�@�Z@�(�@�l�@�o@�=q@�p�@�?}@�%@ċD@�b@�  @��;@�ƨ@Õ�@�33@��@¸R@���@��7@�hs@��`@��D@�r�@�I�@�|�@�+@�o@���@��y@��H@��@�p�@�O�@�7L@��/@���@��@�Z@��@��P@�K�@��@���@�E�@�{@���@��#@��h@�7L@�r�@��w@�;d@�o@�
=@��@��\@�-@�J@��#@�p�@�&�@�%@���@�Ĝ@�9X@���@�S�@�
=@��y@�ȴ@�v�@�@���@��@���@�A�@�1@��@��
@���@�S�@�ȴ@���@��\@�5?@�J@���@�G�@��@��u@�bN@�Q�@�I�@�1'@�1@��
@�
=@�J@�O�@��/@�I�@��;@��;@��w@�|�@�@�V@��@���@�/@��@���@�r�@�Q�@��;@�dZ@�o@���@�V@�5?@�$�@��@��-@��@�Z@���@��w@�|�@�C�@��@���@��R@�@��@�hs@�?}@��@�V@���@��@�Z@�Q�@��@��@���@���@�|�@�C�@�"�@�@��\@�{@���@��-@�?}@���@�bN@�I�@�b@��w@���@��m@���@�A�@�;@v�R@m/@e��@[o@R^5@J�H@CdZ@<�@6�+@.�+@(�u@!�7@�@�@�F@v�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A��A��A��A��A��A��A��A��A�"�A�&�A�(�A�+A�-A�/A�/A�1'A�1'A�5?A�7LA�9XA�;dA�E�A�M�A�I�A�G�A�C�A�-A��A߾wAߛ�A�33A���A��`A޶FAދDA�$�A�E�A�bNA�Q�Aڣ�A�1'A��
A�9XAذ!A���A���A��Aө�A�ȴA�
=AύPA��AͼjA�A�bA�ƨA�n�AʼjAɴ9AȬAǴ9AƉ7A��TAŝ�A�E�A��yA��mA�p�A�A�r�A�I�A�ffA�n�A�bA�"�A�K�A��A�jA���A� �A�n�A��A�x�A���A�
=A�%A�{A�\)A�ȴA�E�A���A���A���A���A�ZA�x�A���A��RA��A��TA��uA���A�hsA��PA��A���A�33A�9XA���A���A�;dA�M�A���A�`BA���A}�Ax��Au;dAo��Aj5?Ae�Ad��Ac��Ab�Aa��A^�9AX�AT��APffAKK�AIƨAG��AD1'A@�DA<��A<A�A8��A5��A3&�A1�hA0z�A.-A,jA+��A)�wA(ZA&�HA&M�A%S�A%�A#|�A#�A#��A#+A"�yA"�RA"�DA"��A"�DA ��A�wA�7AȴAM�A�A��A�yA�A-A�Ap�Al�A�AdZA?}A��AS�A=qA��A�A�AdZA�#A�A=qAƨAr�A"�A+A�`A�yA�+A�A�hA+A��A�jAv�AA��Al�AVA	&�Ar�A�mAI�A�A�AZAJA��A��A�yAbNAVAt�A�PAt�Az�A��A��A�Ar�A�A��AS�A%A ��A $�@�+@�&�@��y@���@��-@�Ĝ@��@�"�@�-@�O�@�j@�b@���@�C�@��@�J@��@�G�@�&�@�I�@�K�@�n�@�G�@��@�%@�Ĝ@�u@�@�7@��`@�  @�K�@�v�@�$�@�{@���@���@�9@�I�@�dZ@�$�@�`B@�1'@��y@��@�X@ܴ9@�1'@� �@�  @۾w@ۍP@�t�@��H@�@��/@�1'@ם�@�l�@�K�@��@�o@���@���@և+@�ff@�V@��@�7L@�z�@� �@��;@�A�@�(�@Ӆ@�\)@�ff@�@�%@�1'@�+@Η�@�V@���@Ͳ-@͡�@͑h@�p�@�%@�z�@��
@�|�@�o@ʏ\@�5?@��#@�`B@ț�@�Z@�(�@�l�@�o@�=q@�p�@�?}@�%@ċD@�b@�  @��;@�ƨ@Õ�@�33@��@¸R@���@��7@�hs@��`@��D@�r�@�I�@�|�@�+@�o@���@��y@��H@��@�p�@�O�@�7L@��/@���@��@�Z@��@��P@�K�@��@���@�E�@�{@���@��#@��h@�7L@�r�@��w@�;d@�o@�
=@��@��\@�-@�J@��#@�p�@�&�@�%@���@�Ĝ@�9X@���@�S�@�
=@��y@�ȴ@�v�@�@���@��@���@�A�@�1@��@��
@���@�S�@�ȴ@���@��\@�5?@�J@���@�G�@��@��u@�bN@�Q�@�I�@�1'@�1@��
@�
=@�J@�O�@��/@�I�@��;@��;@��w@�|�@�@�V@��@���@�/@��@���@�r�@�Q�@��;@�dZ@�o@���@�V@�5?@�$�@��@��-@��@�Z@���@��w@�|�@�C�@��@���@��R@�@��@�hs@�?}@��@�V@���@��@�Z@�Q�@��@��@���@���@�|�@�C�@�"�@�@��\@�{@���@��-@�?}@���@�bN@�I�@�b@��w@���@��m@���@�A�@�;@v�R@m/@e��@[o@R^5@J�H@CdZ@<�@6�+@.�+@(�u@!�7@�@�@�F@v�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
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
!�B
$�B
&�B
(�B
/B
<jB
iyB
{�B
|�B
� B
�B
�7B
�DB
�DB
�DB
�JB
�JB
�JB
�DB
�7B
�VB
�VB
�7B
�7B
��B
��B
�B
�B
�B
��B
�hB
��B
��B
��B
�#B
�`B
��B+B�B�B"�B6FBO�Bq�B}�B��B�FB�dB��BǮB�BJBuB"�B)�B(�B2-BT�BaHBaHBZBS�BP�BO�BM�BG�B;dB;dBC�B%�B"�B>wB:^BH�B@�B.B�B\B��B�yB�#B�wB{�BI�B'�B\B  B
�B
�B
�wB
�B
�VB
y�B
l�B
[#B
M�B
C�B
=qB
-B	�B	��B	�^B	��B	{�B	ffB	_;B	YB	Q�B	J�B	;dB	!�B	VB��B�mB�BB�)B�#B�B�B�B��B�B�TB�ZB�B��B��B	B		7B		7B	{B	#�B	A�B	F�B	P�B	^5B	p�B	{�B	�B	�B	�B	�7B	�DB	�B	~�B	~�B	�B	r�B	o�B	q�B	r�B	t�B	u�B	{�B	�B	�=B	�VB	��B	��B	��B	�bB	�=B	�PB	��B	��B	��B	�3B	�B	��B	��B	��B	�uB	��B	��B	��B	��B	�hB	�7B	�1B	�bB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�FB	�B	�B	�B	�B	�'B	�!B	�B	�3B	�'B	�XB	�jB	�LB	�9B	�-B	�wB	ƨB	��B	��B	��B	��B	ɺB	ȴB	ĜB	�jB	�FB	�?B	�RB	�?B	�-B	�'B	�3B	�!B	�-B	�3B	�RB	�dB	�jB	��B	��B	B	ŢB	ƨB	ŢB	��B	��B	��B	��B	��B	ɺB	ŢB	ÖB	ĜB	B	B	��B	��B	��B	��B	��B	��B	��B	B	ĜB	ŢB	ƨB	ƨB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�)B	�/B	�)B	�)B	�#B	�B	�)B	�5B	�BB	�BB	�BB	�BB	�HB	�HB	�HB	�HB	�HB	�HB	�HB	�NB	�TB	�ZB	�fB	�mB	�mB	�sB	�sB	�sB	�yB	�yB	�sB	�mB	�mB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
%B
B
%B
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
	7B
	7B

=B

=B
DB
DB
JB
JB
JB
JB
DB
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
VB
VB
VB
VB
VB
VB
VB
VB
VB
\B
VB
\B
bB
bB
hB
hB
�B
uB
�B
�B
"�B
'�B
/B
8RB
>wB
A�B
G�B
M�B
O�B
S�B
YB
_;B
dZB
jB
n�B
r�B
v�B
|�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B
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
!�B
$�B
&�B
(�B
.�B
<AB
iOB
{�B
|�B
�B
��B
�B
�B
�B
�B
� B
�B
�!B
�B
�B
�-B
�-B
�B
�
B
�dB
��B
��B
��B
��B
�WB
�>B
�eB
��B
ͧB
��B
�5B
��B B[B�B"�B6BO�Bq{B}�B�nB�B�7B�]BǂB�fBBHB"�B)�B(�B1�BT�BaBaBY�BS�BP�BO�BM�BG�B;8B;6BChB%�B"�B>DB:/BH�B@QB-�BzB*B��B�EB��B�FB{�BI�B'�B&B
��B
�~B
��B
�BB
��B
�!B
y�B
lXB
Z�B
M�B
CbB
==B
,�B	�{B	һB	�*B	�\B	{�B	f0B	_B	X�B	Q�B	J�B	;,B	!�B	B��B�2B�B��B��B��B��B��BҷB��B�B� B�VB��B��B	 �B	�B	�B	?B	#�B	AMB	FkB	P�B	]�B	peB	{�B	��B	��B	��B	��B	�B	��B	~�B	~�B	��B	rrB	o`B	qkB	rqB	t}B	u�B	{�B	��B	��B	�B	�hB	�`B	�MB	�$B	��B	�B	�RB	��B	��B	��B	��B	��B	��B	�LB	�4B	�FB	�YB	�yB	�|B	�(B	��B	��B	�!B	�HB	�YB	�AB	�~B	��B	��B	�kB	�WB	�fB	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�)B	�B	��B	��B	�7B	�gB	ʁB	�B	�B	ˆB	�xB	�qB	�[B	�*B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	�B	�$B	�'B	�AB	�HB	�MB	�`B	�gB	�_B	ʀB	˄B	̌B	̍B	˄B	�wB	�aB	�RB	�ZB	�MB	�NB	�GB	�FB	�IB	�BB	�BB	�AB	�@B	�NB	�YB	�aB	�dB	�eB	�cB	�yB	�B	ˀB	ˁB	˃B	˅B	˅B	˄B	̋B	͑B	͒B	͐B	ΘB	ΗB	ϛB	ϜB	ϝB	ϜB	ТB	ѩB	ѩB	ѩB	ѨB	ӶB	ԼB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�"B	�*B	�*B	�0B	�0B	�0B	�7B	�5B	�.B	�)B	�'B	�(B	�.B	�;B	�;B	�:B	�FB	�NB	�NB	�OB	�LB	�MB	�SB	�TB	�VB	�ZB	�]B	�[B	�^B	�fB	�eB	�fB	�hB	�aB	�mB	�lB	�rB	�mB	�pB	�pB	�qB	�rB	�rB	�rB	�sB	�xB	�wB	�zB	�B	�}B	�xB	�B	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
	�B
	�B

�B
 B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
$B
#G�O�B
1B
KB
rB
"�B
'�B
.�B
8B
>1B
ADB
GkB
M�B
O�B
S�B
X�B
^�B
dB
j;B
nUB
rkB
v�B
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.53 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             201904051007562019040510075620190405100756  AO  ARCAADJP                                                                    20181121125948    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181121125948  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181121125948  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190405100756  IP                  G�O�G�O�G�O�                