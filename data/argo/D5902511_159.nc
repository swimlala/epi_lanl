CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2021-01-28T20:27:36Z creation; 2021-04-29T20:27:09Z DMQC;      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.10   Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_on_dac_decoder_version        $Decoded by SIO: Argo SIO SOLOII V2.2   comment_dmqc_operator         bPRIMARY | https://orcid.org/0000-0003-0805-6570 | John Gilson, Scripps Institution of Oceanography        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    8   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  �  8   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  �  8�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  `  9   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        9x   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    9�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    9�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                  @  9�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    9�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    9�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                  @  9�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                  @  :   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                  @  :T   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    :�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   axis      T      
resolution        >�E�r�_K   
_FillValue        A.�~            :�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    :�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        ?F�k"kmj   
_FillValue        A.�~            :�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   	valid_min         �V�        	valid_max         @V�        axis      Y      
_FillValue        @�i�            :�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    	valid_min         �f�        	valid_max         @f�        axis      X      
_FillValue        @�i�            :�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    :�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    :�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    :�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    :�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    :�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    ;    CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        =    PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     h  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \p   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     h  dL   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     h  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     h  ��   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �`   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     h  �<   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     h  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     h �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 8P   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     h @,   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � _�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     h gp   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   �8   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   �8   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   �8   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T �8   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �,   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �H   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �P   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �p   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �x   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20210128202736  20210429202819  5902511 5902511 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO6810_008521_159                 6810_008521_159                 2C  2C  DD  SOLO_II                         SOLO_II                         8521                            8521                            V2.2; SBE602 27Jul16            V2.2; SBE602 27Jul16            853 853 @�Z/"���@�Z/"���11  @�Z/U�=@�Z/U�=@2F�!�.I@2F�!�.I�e��z9��e��z9�11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?��?��H@8Q�@}p�@�  @�(�@޸R@�p�A  A!G�A,(�A>�RA_\)A�  A��A���A�  A��A�Q�A�Q�A�  B   B(�B  B�
B�
B((�B0  B7�
B@  BH  BO�
BX  B`Q�Bh(�Bp  Bx  B�{B�{B�  B��B��B��B�  B�  B�  B�  B��B��
B��B��B��B��B��B�  B��B��
B��B�  B��B��B��
B��B�{B�  B��B��
B��B��C   C��C  C��C�C
  C�C��C
=C
=C
=C  C��C  C
=C{C {C!��C#�HC%�C(
=C*{C,  C-��C0  C1��C3��C6  C7�C:  C<
=C>  C@  CB  CD  CF  CH
=CJ{CL  CN  CP  CR  CT
=CV  CW��CY�HC\  C^
=C_��Ca��Cd  Ce��Cg��Ci��Ck��Cn  Cp  Cq�Cs��Cu��Cw��Cy��C{��C~  C�  C�  C�  C���C���C���C���C���C���C�  C���C���C�  C�C�C�  C���C���C�  C���C���C�C�  C�  C�  C�  C�  C���C���C�  C�  C���C�  C�  C���C���C���C�
=C�C���C���C�  C���C���C�
=C�  C�  C���C�  C�  C���C�
=C�C�  C�
=C�C�  C�  C�C���C�  C�  C�  C���C�  C�  C���C�C�  C�  C�  C�  C�C�
=C���C�  C���C�  C���C�C�C�  C�
=C�
=C�  C�C���C�  C�C���C�  C�
=C�C���C���C���C�  C�  C���C�  C�  C���C���C���C���C���C���C��C���C���C�  C���C���C�  C�  C�C�C���C���C�  C�C�
=C�  C�  C�
=C�
=C���C�  D   D � D  D}qD  D�D�Dz�D��D� DD�D�qD}qD�D� D  D�D	  D	� D
�D
��D
�qD}qD�qD� D  D� D  D� D  D� D�D��D�D� D�D��D  D��D  D}qD  D��DD� D��Dz�D  D� D�qD}qD�qD}qD  D�D�D}qD��D}qD  D��D  D}qD   D � D!�D!}qD!�qD"z�D"�qD#��D$D$��D%�D%��D&  D&� D'�D'� D(  D(}qD(�qD)}qD)��D*}qD+�D+��D+�qD,� D-D-��D.  D.� D/  D/��D0  D0� D1D1� D2  D2� D3�D3� D3�qD4��D5�D5��D6  D6� D6��D7� D8�D8��D9  D9��D:�D:��D;  D;��D<�D<� D=  D=� D>  D>� D?  D?� D@  D@� DA�DA�DB  DB� DC  DC��DD�DD� DE  DE��DF�DF� DG  DG� DH  DH� DH�qDI}qDJ  DJ� DK�DK� DK�qDL}qDM  DM� DN  DN}qDN��DO}qDO�qDP}qDP�qDQ}qDQ�qDRz�DR��DS}qDT  DT��DU  DU}qDU�qDV� DW  DW� DW��DX}qDY  DY��DZ�DZ��D[D[� D\  D\��D]  D]� D^  D^z�D_  D_�D`�D`}qDa�Da�Db  Db� Dc  Dc� Dd�Dd� Dd��De� Df�Df}qDg  Dg��Dh  Dh}qDi  Di� Dj  Dj}qDj��Dk� Dl  Dlz�Dm  Dm� Dn  Dn��Do  Do��DpDp��Dq  Dq}qDr  Dr��Ds�Ds��Dt  Dtz�Du  Du��Dv�Dv� Dw  Dw}qDw�qDx}qDy  Dy� Dy��Dz}qDz�qD{� D|�D|��D}D}� D~  D~}qD  D� D��D�@ D�� D���D���D�@ D�~�D�� D�HD�>�D�}qD���D���D�=qD�~�D���D�  D�B�D��HD�� D�  D�AHD��HD�D�HD�AHD��HD�D��D�@ D��HD��HD��qD�=qD�~�D�� D�HD�@ D��HD�� D��qD�@ D�� D��HD�HD�@ D�~�D��HD���D�>�D�� D���D���D�@ D�� D�� D�HD�@ D�~�D�� D�HD�@ D�� D���D���D�>�D�~�D���D��qD�@ D�� D�� D�HD�AHD�~�D�� D�HD�AHD���D��HD���D�>�D�� D�� D��D�B�D�� D�� D�HD�AHD��HD�� D��qD�>�D���D�D��D�B�D�� D�� D�  D�@ D�� D���D��qD�>�D��HD��HD���D�=qD�� D���D���D�>�D�~�D�� D�HD�B�D���D��HD�  D�@ D�� D��HD�  D�>�D�~�D�� D���D�>�D�~�D��qD���D�@ D�� D�� D���D�@ D�~�D�� D�  D�>�D�� D�� D�HD�B�D���D��HD�HD�AHD�� D���D�  D�@ D�� D�� D���D�>�D�� D�� D�HD�AHD��HD��HD�  D�>�D�� D��HD�  D�@ D�� D�D��D�@ D�� D��HD�HD�@ D�~�D�� D�  D�AHD��HD�� D�HD�AHD�� D�� D�  D�@ D�~�D���D���D�=qD�~�D�� D��D�@ D�� D��HD�  D�>�D�� D�� D���D�>�D�}qD���D�  D�=qD�}qD�� D�HD�>�D�}qD�� D���D�>�D�� D���D���D�=qD�~�D��HD�  D�>�D�� D�� D�  D�@ D�� D���D���D�AHD�~�D��HD��D�>�D�}qD���D�  D�AHD��HD���D��qD�=qD�~�D�� D�  D�>�D�� D�D��D�AHD�� D��HD���D�>�D�� D�� D�  D�@ D�� D�� D�HD�@ D�~�D�� D�  D�>�D�}qDþ�D�HD�AHD�~�Dľ�D�HD�B�DŁHD�� D�  D�>�DƁHD�� D���D�@ DǁHD��HD�  D�>�DȁHD��HD�HD�AHDɀ D��HD�HD�@ Dʀ D�� D�  D�@ DˁHD�D�HD�AHD́HD̾�D�  D�@ D̀ D��HD�  D�=qD�~�D�� D�  D�@ DρHD�� D�  D�AHDЁHD��HD�HD�@ DсHD�� D���D�AHDҁHDҾ�D�  D�@ DӀ DӾ�D�  D�>�D�~�D�� D�  D�>�D�~�Dվ�D���D�>�Dր D־�D���D�@ D׀ D�� D�  D�AHD؁HD�� D�  D�@ D�~�D�� D�  D�@ Dڀ Dھ�D���D�>�Dۀ D�� D���D�>�D܀ D�� D��qD�>�D݀ Dݾ�D���D�AHDރ�D�D���D�>�D߀ D�� D�  D�@ D��HD��HD�  D�B�DႏD��HD�HD�@ D� D�� D�  D�@ D�~�D��HD�  D�>�D�~�D侸D�  D�AHD�~�D徸D�HD�AHD� D�� D���D�=qD�~�D�� D�  D�AHD肏D��HD���D�>�D� D��HD�HD�AHD�HD꾸D���D�>�D�HD��HD�HD�@ D�~�D쾸D�  D�AHD� D���D�  D�@ D�~�DD��qD�>�D� D��HD�HD�>�D�~�D�� D�  D�>�D� D��HD�HD�@ D� D�� D�HD�AHD�HD��HD�HD�@ D�}qD��qD�  D�B�D��HD���D���D�>�D�~�D���D���D�AHD���D�� D���D�@ D�� D���D�  D�AHD�� D���>�\)>��
?#�
?��?��R?��@   @\)@+�@:�H@O\)@k�@}p�@�ff@�z�@�(�@�ff@�33@�Q�@��
@У�@�Q�@�\@�{@�
=A�A	��Ap�AG�A
=A�RA#�
A(��A/\)A3�
A:=qAAG�AE�AL(�AS�
AXQ�A^�RAfffAl(�Aq�Az=qA�Q�A��HA�ffA�G�A��
A�
=A��\A���A�\)A��HA�A�  A��
A�ffA���A�(�A�
=A���A��
A�\)A�G�A�(�A�\)A���A��
A�\)Aʏ\A�z�A�
=A��HA��A׮A�33A޸RA��A��
A�\)A陚A�(�A�
=A�G�A�A�ffA���A��A���A�
=B Q�B��B�HB�B�B�B�HB(�B��B	B
=B�Bz�B�B�\B\)Bz�Bp�B�B33B(�B��B��B�RB\)B  BG�B=qB�RB�B��BG�BffB\)B   B!G�B"ffB#
=B$(�B%p�B&{B'
=B(Q�B)�B)�B+33B+�
B,��B-B.�RB/33B0z�B1p�B2{B3\)B4(�B4��B6{B733B7�B8��B9�B:ffB;�B<��B=G�B>=qB?�B@Q�B@��BB=qBC33BC�
BD��BE�BF�\BG�BHz�BH��BJ{BK33BL  BL��BN{BN�RBO\)BP��BQp�BR=qBS\)BT(�BT��BU�BV�HBW�BXz�BYp�BZ�\B[33B[�
B]�B^{B^�\B_�B`��BaG�Bb{Bc\)Bd  Bd��Be�Bf�\Bg33Bhz�BiG�BiBj�HBl  Bl��BmG�BnffBo\)Bo�
Bp��BqBr�\Bs33Btz�Bu�BuBv�HBw�BxQ�ByG�BzffB{
=B{�B|��B}��B~=qB~�HB�{B�Q�B��RB�\)B���B��B�z�B���B�33B���B�(�B���B���B�\)B��B�{B��\B��B�\)B�B�Q�B��RB���B��B��B�(�B���B�
=B�p�B�  B�ffB��RB�33B��B�  B�ffB�
=B�\)B��B�{B���B���B�p�B�  B�=qB���B�G�B��B�  B��\B�
=B�\)B�B�ffB��RB��B�B�{B�z�B�
=B��B��
B�Q�B��HB�\)B��B�=qB���B��B��B�=qB��\B���B��B�{B�ffB��HB�p�B��B�=qB���B�\)B�B�(�B��RB�G�B���B�{B��RB��B��B�  B���B�
=B�p�B�{B���B���B�p�B�  B��\B��HB�p�B�{B�z�B��HB�p�B�  B�ffB��RB�p�B�  B�Q�B���B�p�B�  B�Q�B��HB��B�{B�z�B��HB��B�{B�z�B���B���B�(�B���B���B��B�=qB��RB��B��B�Q�B���B�33B��
B�ffB���B�G�B��B\B���B�p�B�{Bģ�B���BŅB�=qBƸRB�
=BǅB�=qBȸRB��B��
B�Q�BʸRB�G�B��B�z�B��HB�\)B�  BΏ\B���B�p�B�(�BЏ\B���BхB�=qBң�B�
=BӮB�Q�B���B�33B��
B�z�B���B�G�B��B؏\B��HB�\)B�  Bڏ\B��HB�\)B�  B�z�B���B�G�B��B�ffB���B�33B��
B�ffB���B�G�B�  B�z�B��HB�p�B�(�B��B�
=B噚B�Q�B��HB�G�B�B�ffB���B�p�B��B�ffB��B뙚B�  B�ffB�
=B홚B�  B�Q�B���B�B��B�=qB���B�\)B��
B�=qB�RB�\)B��
B�(�B���B�G�B��
B�(�B���B��B�B�=qB���B�
=B���B�(�B�z�B��HB�p�B�  B�z�B���B�G�B��
B�ffB��RB�33B��
C (�C G�C �\C �
C{C=qCz�C��C  C(�CffC�C�C�CG�C�C��C  C(�CffC�RC�C
=CG�C��C�
C  C33Cz�CC
=C(�C\)C�C��C(�C\)C��C�C	(�C	Q�C	��C	�C
(�C
Q�C
��C
�HC{C=qC�C��C
=C33Cp�C�C  C33CffC�C  C33Cp�C��C  C=qCz�C�C�C=qC�\C��C  C33Cz�C�
C{C=qC�C�HC
=CG�C��C�HC
=CQ�C�C�C�C\)C�C��C(�Cp�CC  C=qCp�C��C
=C=qCp�C�
C
=C=qC�\C�
C  CQ�C��C�
C
=C\)C��C�
C33Cz�C�C�C33C�\C��C  C\)C�C�HC(�Cz�C��C   C Q�C �C ��C!(�C!z�C!�
C"{C"Q�C"�RC#
=C#=qC#��C#��C$(�C$p�C$�
C%�C%\)C%�C&
=C&\)C&��C&�
C'=qC'�\C'C(�C(z�C(�C(��C)Q�C)�C)�C*33C*��C*�HC+�C+z�C+�
C,
=C,ffC,C-  C-G�C-��C.  C.G�C.�\C.�
C/33C/�\C/�
C0�C0�C0�
C1{C1\)C1C2{C2Q�C2��C3  C3G�C3�\C3�
C433C4z�C4C5  C5ffC5�RC5��C6G�C6��C6�C733C7p�C7��C8�C8\)C8�C9
=C9Q�C9�\C9�C:=qC:�C:��C;  C;\)C;�RC;��C<33C<��C<�HC=�C=ffC=C>
=C>=qC>�\C>�C?=qC?z�C?�RC@�C@ffC@��C@�CA=qCA�\CA�
CB�CBffCB�RCC{CCQ�CC��CC�HCD=qCD�\CD�
CE{CEffCECF
=CFG�CF��CF��CG=qCGz�CG�
CH(�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                      ?��?��H@8Q�@}p�@�  @�(�@޸R@�p�A  A!G�A,(�A>�RA_\)A�  A��A���A�  A��A�Q�A�Q�A�  B   B(�B  B�
B�
B((�B0  B7�
B@  BH  BO�
BX  B`Q�Bh(�Bp  Bx  B�{B�{B�  B��B��B��B�  B�  B�  B�  B��B��
B��B��B��B��B��B�  B��B��
B��B�  B��B��B��
B��B�{B�  B��B��
B��B��C   C��C  C��C�C
  C�C��C
=C
=C
=C  C��C  C
=C{C {C!��C#�HC%�C(
=C*{C,  C-��C0  C1��C3��C6  C7�C:  C<
=C>  C@  CB  CD  CF  CH
=CJ{CL  CN  CP  CR  CT
=CV  CW��CY�HC\  C^
=C_��Ca��Cd  Ce��Cg��Ci��Ck��Cn  Cp  Cq�Cs��Cu��Cw��Cy��C{��C~  C�  C�  C�  C���C���C���C���C���C���C�  C���C���C�  C�C�C�  C���C���C�  C���C���C�C�  C�  C�  C�  C�  C���C���C�  C�  C���C�  C�  C���C���C���C�
=C�C���C���C�  C���C���C�
=C�  C�  C���C�  C�  C���C�
=C�C�  C�
=C�C�  C�  C�C���C�  C�  C�  C���C�  C�  C���C�C�  C�  C�  C�  C�C�
=C���C�  C���C�  C���C�C�C�  C�
=C�
=C�  C�C���C�  C�C���C�  C�
=C�C���C���C���C�  C�  C���C�  C�  C���C���C���C���C���C���C��C���C���C�  C���C���C�  C�  C�C�C���C���C�  C�C�
=C�  C�  C�
=C�
=C���C�  D   D � D  D}qD  D�D�Dz�D��D� DD�D�qD}qD�D� D  D�D	  D	� D
�D
��D
�qD}qD�qD� D  D� D  D� D  D� D�D��D�D� D�D��D  D��D  D}qD  D��DD� D��Dz�D  D� D�qD}qD�qD}qD  D�D�D}qD��D}qD  D��D  D}qD   D � D!�D!}qD!�qD"z�D"�qD#��D$D$��D%�D%��D&  D&� D'�D'� D(  D(}qD(�qD)}qD)��D*}qD+�D+��D+�qD,� D-D-��D.  D.� D/  D/��D0  D0� D1D1� D2  D2� D3�D3� D3�qD4��D5�D5��D6  D6� D6��D7� D8�D8��D9  D9��D:�D:��D;  D;��D<�D<� D=  D=� D>  D>� D?  D?� D@  D@� DA�DA�DB  DB� DC  DC��DD�DD� DE  DE��DF�DF� DG  DG� DH  DH� DH�qDI}qDJ  DJ� DK�DK� DK�qDL}qDM  DM� DN  DN}qDN��DO}qDO�qDP}qDP�qDQ}qDQ�qDRz�DR��DS}qDT  DT��DU  DU}qDU�qDV� DW  DW� DW��DX}qDY  DY��DZ�DZ��D[D[� D\  D\��D]  D]� D^  D^z�D_  D_�D`�D`}qDa�Da�Db  Db� Dc  Dc� Dd�Dd� Dd��De� Df�Df}qDg  Dg��Dh  Dh}qDi  Di� Dj  Dj}qDj��Dk� Dl  Dlz�Dm  Dm� Dn  Dn��Do  Do��DpDp��Dq  Dq}qDr  Dr��Ds�Ds��Dt  Dtz�Du  Du��Dv�Dv� Dw  Dw}qDw�qDx}qDy  Dy� Dy��Dz}qDz�qD{� D|�D|��D}D}� D~  D~}qD  D� D��D�@ D�� D���D���D�@ D�~�D�� D�HD�>�D�}qD���D���D�=qD�~�D���D�  D�B�D��HD�� D�  D�AHD��HD�D�HD�AHD��HD�D��D�@ D��HD��HD��qD�=qD�~�D�� D�HD�@ D��HD�� D��qD�@ D�� D��HD�HD�@ D�~�D��HD���D�>�D�� D���D���D�@ D�� D�� D�HD�@ D�~�D�� D�HD�@ D�� D���D���D�>�D�~�D���D��qD�@ D�� D�� D�HD�AHD�~�D�� D�HD�AHD���D��HD���D�>�D�� D�� D��D�B�D�� D�� D�HD�AHD��HD�� D��qD�>�D���D�D��D�B�D�� D�� D�  D�@ D�� D���D��qD�>�D��HD��HD���D�=qD�� D���D���D�>�D�~�D�� D�HD�B�D���D��HD�  D�@ D�� D��HD�  D�>�D�~�D�� D���D�>�D�~�D��qD���D�@ D�� D�� D���D�@ D�~�D�� D�  D�>�D�� D�� D�HD�B�D���D��HD�HD�AHD�� D���D�  D�@ D�� D�� D���D�>�D�� D�� D�HD�AHD��HD��HD�  D�>�D�� D��HD�  D�@ D�� D�D��D�@ D�� D��HD�HD�@ D�~�D�� D�  D�AHD��HD�� D�HD�AHD�� D�� D�  D�@ D�~�D���D���D�=qD�~�D�� D��D�@ D�� D��HD�  D�>�D�� D�� D���D�>�D�}qD���D�  D�=qD�}qD�� D�HD�>�D�}qD�� D���D�>�D�� D���D���D�=qD�~�D��HD�  D�>�D�� D�� D�  D�@ D�� D���D���D�AHD�~�D��HD��D�>�D�}qD���D�  D�AHD��HD���D��qD�=qD�~�D�� D�  D�>�D�� D�D��D�AHD�� D��HD���D�>�D�� D�� D�  D�@ D�� D�� D�HD�@ D�~�D�� D�  D�>�D�}qDþ�D�HD�AHD�~�Dľ�D�HD�B�DŁHD�� D�  D�>�DƁHD�� D���D�@ DǁHD��HD�  D�>�DȁHD��HD�HD�AHDɀ D��HD�HD�@ Dʀ D�� D�  D�@ DˁHD�D�HD�AHD́HD̾�D�  D�@ D̀ D��HD�  D�=qD�~�D�� D�  D�@ DρHD�� D�  D�AHDЁHD��HD�HD�@ DсHD�� D���D�AHDҁHDҾ�D�  D�@ DӀ DӾ�D�  D�>�D�~�D�� D�  D�>�D�~�Dվ�D���D�>�Dր D־�D���D�@ D׀ D�� D�  D�AHD؁HD�� D�  D�@ D�~�D�� D�  D�@ Dڀ Dھ�D���D�>�Dۀ D�� D���D�>�D܀ D�� D��qD�>�D݀ Dݾ�D���D�AHDރ�D�D���D�>�D߀ D�� D�  D�@ D��HD��HD�  D�B�DႏD��HD�HD�@ D� D�� D�  D�@ D�~�D��HD�  D�>�D�~�D侸D�  D�AHD�~�D徸D�HD�AHD� D�� D���D�=qD�~�D�� D�  D�AHD肏D��HD���D�>�D� D��HD�HD�AHD�HD꾸D���D�>�D�HD��HD�HD�@ D�~�D쾸D�  D�AHD� D���D�  D�@ D�~�DD��qD�>�D� D��HD�HD�>�D�~�D�� D�  D�>�D� D��HD�HD�@ D� D�� D�HD�AHD�HD��HD�HD�@ D�}qD��qD�  D�B�D��HD���D���D�>�D�~�D���D���D�AHD���D�� D���D�@ D�� D���D�  D�AHD�� G�O�>�\)>��
?#�
?��?��R?��@   @\)@+�@:�H@O\)@k�@}p�@�ff@�z�@�(�@�ff@�33@�Q�@��
@У�@�Q�@�\@�{@�
=A�A	��Ap�AG�A
=A�RA#�
A(��A/\)A3�
A:=qAAG�AE�AL(�AS�
AXQ�A^�RAfffAl(�Aq�Az=qA�Q�A��HA�ffA�G�A��
A�
=A��\A���A�\)A��HA�A�  A��
A�ffA���A�(�A�
=A���A��
A�\)A�G�A�(�A�\)A���A��
A�\)Aʏ\A�z�A�
=A��HA��A׮A�33A޸RA��A��
A�\)A陚A�(�A�
=A�G�A�A�ffA���A��A���A�
=B Q�B��B�HB�B�B�B�HB(�B��B	B
=B�Bz�B�B�\B\)Bz�Bp�B�B33B(�B��B��B�RB\)B  BG�B=qB�RB�B��BG�BffB\)B   B!G�B"ffB#
=B$(�B%p�B&{B'
=B(Q�B)�B)�B+33B+�
B,��B-B.�RB/33B0z�B1p�B2{B3\)B4(�B4��B6{B733B7�B8��B9�B:ffB;�B<��B=G�B>=qB?�B@Q�B@��BB=qBC33BC�
BD��BE�BF�\BG�BHz�BH��BJ{BK33BL  BL��BN{BN�RBO\)BP��BQp�BR=qBS\)BT(�BT��BU�BV�HBW�BXz�BYp�BZ�\B[33B[�
B]�B^{B^�\B_�B`��BaG�Bb{Bc\)Bd  Bd��Be�Bf�\Bg33Bhz�BiG�BiBj�HBl  Bl��BmG�BnffBo\)Bo�
Bp��BqBr�\Bs33Btz�Bu�BuBv�HBw�BxQ�ByG�BzffB{
=B{�B|��B}��B~=qB~�HB�{B�Q�B��RB�\)B���B��B�z�B���B�33B���B�(�B���B���B�\)B��B�{B��\B��B�\)B�B�Q�B��RB���B��B��B�(�B���B�
=B�p�B�  B�ffB��RB�33B��B�  B�ffB�
=B�\)B��B�{B���B���B�p�B�  B�=qB���B�G�B��B�  B��\B�
=B�\)B�B�ffB��RB��B�B�{B�z�B�
=B��B��
B�Q�B��HB�\)B��B�=qB���B��B��B�=qB��\B���B��B�{B�ffB��HB�p�B��B�=qB���B�\)B�B�(�B��RB�G�B���B�{B��RB��B��B�  B���B�
=B�p�B�{B���B���B�p�B�  B��\B��HB�p�B�{B�z�B��HB�p�B�  B�ffB��RB�p�B�  B�Q�B���B�p�B�  B�Q�B��HB��B�{B�z�B��HB��B�{B�z�B���B���B�(�B���B���B��B�=qB��RB��B��B�Q�B���B�33B��
B�ffB���B�G�B��B\B���B�p�B�{Bģ�B���BŅB�=qBƸRB�
=BǅB�=qBȸRB��B��
B�Q�BʸRB�G�B��B�z�B��HB�\)B�  BΏ\B���B�p�B�(�BЏ\B���BхB�=qBң�B�
=BӮB�Q�B���B�33B��
B�z�B���B�G�B��B؏\B��HB�\)B�  Bڏ\B��HB�\)B�  B�z�B���B�G�B��B�ffB���B�33B��
B�ffB���B�G�B�  B�z�B��HB�p�B�(�B��B�
=B噚B�Q�B��HB�G�B�B�ffB���B�p�B��B�ffB��B뙚B�  B�ffB�
=B홚B�  B�Q�B���B�B��B�=qB���B�\)B��
B�=qB�RB�\)B��
B�(�B���B�G�B��
B�(�B���B��B�B�=qB���B�
=B���B�(�B�z�B��HB�p�B�  B�z�B���B�G�B��
B�ffB��RB�33B��
C (�C G�C �\C �
C{C=qCz�C��C  C(�CffC�C�C�CG�C�C��C  C(�CffC�RC�C
=CG�C��C�
C  C33Cz�CC
=C(�C\)C�C��C(�C\)C��C�C	(�C	Q�C	��C	�C
(�C
Q�C
��C
�HC{C=qC�C��C
=C33Cp�C�C  C33CffC�C  C33Cp�C��C  C=qCz�C�C�C=qC�\C��C  C33Cz�C�
C{C=qC�C�HC
=CG�C��C�HC
=CQ�C�C�C�C\)C�C��C(�Cp�CC  C=qCp�C��C
=C=qCp�C�
C
=C=qC�\C�
C  CQ�C��C�
C
=C\)C��C�
C33Cz�C�C�C33C�\C��C  C\)C�C�HC(�Cz�C��C   C Q�C �C ��C!(�C!z�C!�
C"{C"Q�C"�RC#
=C#=qC#��C#��C$(�C$p�C$�
C%�C%\)C%�C&
=C&\)C&��C&�
C'=qC'�\C'C(�C(z�C(�C(��C)Q�C)�C)�C*33C*��C*�HC+�C+z�C+�
C,
=C,ffC,C-  C-G�C-��C.  C.G�C.�\C.�
C/33C/�\C/�
C0�C0�C0�
C1{C1\)C1C2{C2Q�C2��C3  C3G�C3�\C3�
C433C4z�C4C5  C5ffC5�RC5��C6G�C6��C6�C733C7p�C7��C8�C8\)C8�C9
=C9Q�C9�\C9�C:=qC:�C:��C;  C;\)C;�RC;��C<33C<��C<�HC=�C=ffC=C>
=C>=qC>�\C>�C?=qC?z�C?�RC@�C@ffC@��C@�CA=qCA�\CA�
CB�CBffCB�RCC{CCQ�CC��CC�HCD=qCD�\CD�
CE{CEffCECF
=CFG�CF��CF��CG=qCGz�CG�
CH(�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                      @�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��#A��;A��/A��;A��HA��HA��TA��TA���A��A��A��A��A��A��A��A��A��A��A��A���A���A�
=A�bA��A� �A�&�A�/A�`BA�t�A�x�A�t�A�XA�Q�A�VA�\)A�hsA�hsA�r�A�v�A�p�A�l�A�ffA�dZA�bNA�bNA�bNA�VA�K�A�G�A�9XA�5?A�33A�33A�1'A�9XA�M�A�VA�Q�A�=qA�&�A�~�A�5?A��A�1A�A�O�A�7LAƏ\A�VA�`BA7A��
A��wA���A���A�
=A�VA�hsA�A���A�bA��A��7A���A�?}A���A���A��mA��7A��wA�A�bNA��`A��wA���A���A�VA�A��uA�bA��A�=qA�XA�K�A�ƨA��DA�JA��A�dZA���A�ȴA���A�p�A���A�`BA��A��A�hsA�
=A���A�oA���A��#A�Q�A���A���A��/A�
=A�A���A~1'Ax1'Av�uAup�Ar�/Ap�\An��AlA�Ai�PAf�Acx�A`ȴA_+A\��A[ƨAX�/AV�+AR  AP$�AO��AM��AJ��AH�!AF�HAFAEACoAAK�A>�+A=��A<  A9
=A7|�A6~�A5��A4��A2�A0�/A/G�A-A,��A+33A)XA&�uA$ĜA$��A$�A"�9A!��A!�A �RA n�A�Ap�A�!Al�A�;A7LA��A�mA?}A��A�A��AS�A�jA�A33AVA�A��A\)AG�A�AA�A+Av�A`BA��AjA�#A�FA��A\)A
�!A
bNA	A��A�mAC�A%AffA$�At�A"�A�`A�mA�uAƨA7LAA ��@���@�7L@��w@��@��@��@�X@�  @�K�@�V@�33@�n�@��@�t�@�+@�@�hs@�j@��@�^5@�hs@���@��
@�l�@�o@�K�@���@��@�@ݩ�@���@�(�@�"�@٩�@�/@���@�A�@�1'@�b@��;@�ƨ@׶F@�\)@�o@���@�$�@�V@Ԭ@�z�@� �@��;@ӍP@��H@�V@�=q@���@�V@���@Л�@�r�@�bN@�9X@�C�@Ώ\@�n�@�@̃@�b@�t�@��y@�o@�-@���@���@�&�@�Q�@ǅ@ƸR@�-@�p�@�V@��@��@öF@Ý�@��@§�@�^5@���@���@��`@���@��@�r�@�1@��@��@�"�@���@��#@�G�@���@��j@�(�@��m@�9X@� �@��F@�C�@�o@��@���@���@��@��@���@��^@��#@�@��7@���@�Ĝ@��j@� �@���@��@��@�t�@�dZ@�K�@�;d@��@�ȴ@�^5@�=q@�=q@���@�$�@�J@���@���@�O�@��D@���@�S�@�;d@�K�@�K�@�C�@�33@��R@���@��@��;@���@��F@�9X@�9X@�9X@�9X@��@�ƨ@�+@��@�ȴ@�E�@�@�hs@���@���@�Q�@��;@�ȴ@���@���@�ȴ@��H@��H@�ȴ@�ff@��#@�`B@�?}@��@���@��`@���@��@��D@�(�@���@�K�@�C�@��@��R@���@��R@�n�@�E�@���@��T@��#@���@��7@�hs@�7L@�V@��`@���@��@�bN@�A�@���@���@��@���@��!@�=q@�@�@���@�p�@��@��@�Q�@�A�@�1@��m@���@��F@�33@�o@��@�ȴ@���@�{@�@�?}@���@���@���@��u@�I�@��@�t�@�;d@�@��H@���@���@�~�@���@�V@��`@��u@�b@��w@���@��F@���@�+@��R@��@���@��u@�9X@� �@�1@���@��;@��F@���@�l�@�"�@�@��@�@��h@�&�@�p�@���@�`B@�V@��D@�A�@��@��P@�l�@�\)@��@���@��y@�ȴ@��+@��@�`B@�/@���@�j@��m@��@���@���@�^5@�$�@�@���@�x�@��`@�Ĝ@��D@�z�@���@�33@��@��H@��@���@�ȴ@��!@�~�@�E�@�J@�@��-@���@���@��@�G�@��/@���@��u@��@�ƨ@��F@�;d@���@���@�=q@���@�7L@���@��j@��j@��@���@�z�@�(�@|�@}��@|��@{t�@z��@z�!@z=q@y�@yG�@x��@x �@w�;@w�@w+@v�y@v��@v�@v��@vv�@vff@v{@u�-@up�@u�@t��@tI�@s�F@so@r-@qX@qG�@qhs@qhs@p��@p  @o�@n�@nȴ@nv�@m��@m`B@m?}@l��@lj@l�@k�m@k��@k@j�!@j��@jM�@i7L@h��@hr�@h �@g�;@g��@g�@g\)@f��@f�+@f$�@e��@eO�@d�D@dI�@d�@c�
@ct�@c"�@b�H@b��@b�\@b~�@b^5@b-@a��@a�7@`��@`bN@_��@_l�@_
=@^��@^{@]�@]`B@]�@\�@\j@[�m@[�@[S�@[o@Y��@Y%@XĜ@X�u@XbN@W�;@W;d@V��@V�R@VE�@U�@U�-@T�@TI�@SdZ@R�@R��@R�!@R�!@Rn�@RJ@Q��@Q&�@Q%@P�`@P��@P�u@Pb@O�w@O\)@N�y@N�R@N��@N5?@M�@M�@M�@M/@L�@Lj@L9X@K��@K�F@K��@K��@K�@KdZ@Ko@J-@I�7@H�`@H1'@G�@G�@Fȴ@Fff@FV@FV@FV@F@E�@E�@D��@D�D@DZ@D1@C��@CC�@C"�@B��@B��@Bn�@B�@A��@A�#@@�`@@�u@@bN@@ �@?
=@>ff@>E�@=��@<�@<Z@<1@;��@;��@;�
@;�F@;��@;�@;S�@;C�@;33@;o@:�H@:�!@:n�@9�7@9%@8��@8Q�@8A�@8b@7��@7��@7K�@7+@7
=@6ȴ@6V@6$�@6{@5�-@5�@5V@4��@4��@4�@4I�@3��@3�
@3��@3S�@2��@1��@17L@1�@1%@0��@0��@0�`@0��@0A�@/�;@/�P@/\)@/\)@/\)@/
=@.��@.@-@-��@-?}@-�@,�j@,Z@+�
@+�F@+o@*^5@)��@)x�@)7L@(�9@( �@'�w@'��@'��@'|�@';d@&�R@&��@&��@&v�@%��@%p�@%`B@%`B@%�@$�@$��@$��@$z�@$Z@$9X@$(�@#��@#�m@#dZ@#33@"��@"��@"�\@"J@!��@ ��@ Q�@ Q�@ Q�@ Q�@ Q�@ Q�@ Q�@ Q�@ bN@ bN@ bN@  �@   @��@��@\)@
=@�R@��@��@�+@v�@V@@�-@p�@�@��@�@�j@z�@(�@�m@�F@��@�@S�@�H@��@��@�\@~�@^5@^5@�@J@��@�@��@��@�7@X@��@�@�@�@Q�@b@�;@�@+@�R@��@�+@{@�@��@�-@�h@p�@`B@?}@V@�j@j@9X@(�@��@��@C�@�H@=q@��@�#@�7@��@�@Q�@�@��@�@�@��@�@`B@V@��@��@�@��@�D@j@Z@Z@I�@9X@9X@1@1@�m@�m@�m@�
@�F@��@�@S�@33@33@33@"�@"�@"�@"�@@
��@
~�@
�@	�^@	��@	G�@	7L@	&�@	&�@	%@�`@Ĝ@�@bN@Q�@1'A���A���A��A��/A��A��;A��HA��#A��/A��/A��#A��HA��;A��#A��TA��;A��;A��TA��;A��HA��TA��;A��TA��`A��HA��HA��A���A��A���A��A��A���A��A��A��A��A��A��A���A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A���A��A��A���A��A��A��A���A��A��A��A���A��A��A���A���A���A���A���A��A���A���A��A���A���A��A���A���A���A�  A�A�VA�oA�JA�
=A�oA�bA�bA�oA�VA�VA�oA�{A�VA�{A�{A�bA�oA��A�oA�oA��A��A��A��A��A��A��A��A��A�(�A�(�A�$�A�$�A�$�A�"�A�"�A�(�A�&�A�$�A�-A�(�A�(�A�33A�33A�(�A�-A�/A�-A�9XA�ZA�XA�^5A�`BA�ZA�\)A�bNA�ffA�l�A�v�A�r�A�p�A�v�A�v�A�r�A�x�A�x�A�t�A�t�A�z�A�t�A�x�A�z�A�v�A�x�A�|�A�v�A�v�A�z�A�v�A�p�A�v�A�t�A�l�A�l�A�hsA�bNA�^5A�ZA�O�A�Q�A�VA�VA�Q�A�O�A�S�A�VA�O�A�S�A�VA�O�A�M�A�S�A�O�A�Q�A�VA�S�A�Q�A�XA�VA�VA�XA�\)A�XA�VA�ZA�\)A�VA�ZA�bNA�`BA�^5A�dZA�hsA�hsA�n�A�l�A�hsA�hsA�jA�ffA�dZA�ffA�hsA�dZA�dZA�hsA�dZA�ffA�l�A�jA�jA�p�A�r�A�l�A�n�A�t�A�v�A�r�A�v�A�z�A�v�A�x�A�|�A�x�A�v�A�x�A�x�A�r�A�v�A�t�A�r�A�t�A�p�A�n�A�r�A�r�A�n�A�p�A�r�A�l�A�l�A�p�A�n�A�jA�l�A�n�A�hsA�hsA�jA�ffA�dZA�hsA�ffA�dZA�hsA�hsA�dZA�dZA�hsA�dZA�bNA�ffA�dZA�bNA�ffA�dZA�`BA�bNA�dZA�bNA�^5A�bNA�bNA�^5A�`BA�dZA�^5A�^5A�bNA�dZA�`BA�`BA�bNA�dZA�^5A�`BA�dZA�dZA�`BA�dZA�ffA�`BA�`BA�`BA�ZA�S�A�Q�A�VA�O�A�K�A�M�A�O�A�K�A�K�A�O�A�O�A�I�A�I�A�K�A�I�A�G�A�I�A�K�A�E�A�?}A�C�A�A�A�?}A�7LA�;dA�;dA�5?A�33A�7LA�7LA�33A�1'A�5?A�7LA�5?A�1'A�5?A�7LA�33A�/A�33A�5?A�33A�1'A�33A�7LA�33A�/A�33A�33A�1'A�/A�1'A�33A�/A�-A�1'A�33A�-A�/A�33A�5?A�33A�5?A�;dA�9XA�=qA�E�A�I�A�I�A�K�A�O�A�Q�A�O�A�Q�A�S�A�VA�Q�A�S�A�ZA�ZA�VA�XA�\)A�ZA�VA�S�A�Q�A�M�A�E�A�C�A�C�A�A�A�;dA�?}A�A�A�9XA�9XA�;dA�9XA�1'A�-A�(�A�(�A��A�bA�A���A��TAҾwA�x�A�VA�7LA��HA���AѬAэPA�K�A�VA�A��`AжFA�bNA�E�A�ȴA·+A�K�A�?}A�33A��A��A�oA�VA�A�  A�  A���A���A��A��yA��mA��/Aͺ^A͉7ÁA�z�A�Q�A��
A�$�AˍPA�jA�(�Aɣ�A�t�A�9XA���Aȗ�A�VA���Aǡ�AǅAǁA�t�A�K�A�A��Aư!AƃA�$�AŬA�^5AĶFA�bNA�1A��A��HA���Aú^AöFAá�AÏ\A�|�A�v�A�jA�Q�A�;dA�/A�JA��A���A�ĜA¥�A�A�~�A�O�A�+A�{A�bA�1A���A��mA�ƨA��^A��9A���A�|�A�G�A��A�ȴA��RA���A�v�A�G�A�7LA�$�A���A���A���A�v�A�`BA�I�A�"�A�oA�
=A���A��mA��
A�A���A���A��A�ffA�33A�{A���A��HA��
A���A��!A��A�jA�S�A�1A���A���A��-A���A��hA��A�t�A�jA�Q�A�K�A�G�A�7LA�(�A��A�%A���A��A��mA��HA��#A��
A��A���A���A��jA��A���A��hA�p�A�?}A��A��wA���A���A�~�A�S�A�&�A�-A��A�%A���A��A��-A�^5A��A��9A�r�A��#A���A�bNA�5?A�bA��A��yA��`A��mA���A��jA���A��A�VA�33A��yA�\)A�A���A���A��+A�\)A�E�A�"�A��/A��jA���A��DA�l�A�M�A�33A�oA��A���A�A��RA��!A���A���A��PA��\A��DA�n�A�`BA�K�A�  A��#A���A��9A���A�;dA�ƨA�/A�`BA��HA��
A��!A��hA�|�A�dZA�G�A�-A��A�oA���A��HA���A���A���A�ĜA�A�ĜA�A��^A��RA��RA��!A��A���A���A���A���A���A���A���A���A���A���A��\A�t�A�jA�ffA�XA�;dA�+A�(�A��A�%A�A���A��A��mA��/A���A��jA��hA�v�A�ffA�VA�C�A�5?A�&�A��A�A��A��jA���A�oA��A���A���A��PA�t�A�hsA�ZA�E�A�9XA�-A�
=A���A��A��hA�jA�E�A�bA��`A��RA���A�v�A�K�A� �A���A��`A��A���A�ƨA�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                      A��#A��;A��/A��;A��HA��HA��TA��TA���A��A��A��A��A��A��A��A��A��A��A��A���A���A�
=A�bA��A� �A�&�A�/A�`BA�t�A�x�A�t�A�XA�Q�A�VA�\)A�hsA�hsA�r�A�v�A�p�A�l�A�ffA�dZA�bNA�bNA�bNA�VA�K�A�G�A�9XA�5?A�33A�33A�1'A�9XA�M�A�VA�Q�A�=qA�&�A�~�A�5?A��A�1A�A�O�A�7LAƏ\A�VA�`BA7A��
A��wA���A���A�
=A�VA�hsA�A���A�bA��A��7A���A�?}A���A���A��mA��7A��wA�A�bNA��`A��wA���A���A�VA�A��uA�bA��A�=qA�XA�K�A�ƨA��DA�JA��A�dZA���A�ȴA���A�p�A���A�`BA��A��A�hsA�
=A���A�oA���A��#A�Q�A���A���A��/A�
=A�A���A~1'Ax1'Av�uAup�Ar�/Ap�\An��AlA�Ai�PAf�Acx�A`ȴA_+A\��A[ƨAX�/AV�+AR  AP$�AO��AM��AJ��AH�!AF�HAFAEACoAAK�A>�+A=��A<  A9
=A7|�A6~�A5��A4��A2�A0�/A/G�A-A,��A+33A)XA&�uA$ĜA$��A$�A"�9A!��A!�A �RA n�A�Ap�A�!Al�A�;A7LA��A�mA?}A��A�A��AS�A�jA�A33AVA�A��A\)AG�A�AA�A+Av�A`BA��AjA�#A�FA��A\)A
�!A
bNA	A��A�mAC�A%AffA$�At�A"�A�`A�mA�uAƨA7LAA ��@���@�7L@��w@��@��@��@�X@�  @�K�@�V@�33@�n�@��@�t�@�+@�@�hs@�j@��@�^5@�hs@���@��
@�l�@�o@�K�@���@��@�@ݩ�@���@�(�@�"�@٩�@�/@���@�A�@�1'@�b@��;@�ƨ@׶F@�\)@�o@���@�$�@�V@Ԭ@�z�@� �@��;@ӍP@��H@�V@�=q@���@�V@���@Л�@�r�@�bN@�9X@�C�@Ώ\@�n�@�@̃@�b@�t�@��y@�o@�-@���@���@�&�@�Q�@ǅ@ƸR@�-@�p�@�V@��@��@öF@Ý�@��@§�@�^5@���@���@��`@���@��@�r�@�1@��@��@�"�@���@��#@�G�@���@��j@�(�@��m@�9X@� �@��F@�C�@�o@��@���@���@��@��@���@��^@��#@�@��7@���@�Ĝ@��j@� �@���@��@��@�t�@�dZ@�K�@�;d@��@�ȴ@�^5@�=q@�=q@���@�$�@�J@���@���@�O�@��D@���@�S�@�;d@�K�@�K�@�C�@�33@��R@���@��@��;@���@��F@�9X@�9X@�9X@�9X@��@�ƨ@�+@��@�ȴ@�E�@�@�hs@���@���@�Q�@��;@�ȴ@���@���@�ȴ@��H@��H@�ȴ@�ff@��#@�`B@�?}@��@���@��`@���@��@��D@�(�@���@�K�@�C�@��@��R@���@��R@�n�@�E�@���@��T@��#@���@��7@�hs@�7L@�V@��`@���@��@�bN@�A�@���@���@��@���@��!@�=q@�@�@���@�p�@��@��@�Q�@�A�@�1@��m@���@��F@�33@�o@��@�ȴ@���@�{@�@�?}@���@���@���@��u@�I�@��@�t�@�;d@�@��H@���@���@�~�@���@�V@��`@��u@�b@��w@���@��F@���@�+@��R@��@���@��u@�9X@� �@�1@���@��;@��F@���@�l�@�"�@�@��@�@��h@�&�@�p�@���@�`B@�V@��D@�A�@��@��P@�l�@�\)@��@���@��y@�ȴ@��+@��@�`B@�/@���@�j@��m@��@���@���@�^5@�$�@�@���@�x�@��`@�Ĝ@��D@�z�@���@�33@��@��H@��@���@�ȴ@��!@�~�@�E�@�J@�@��-@���@���@��@�G�@��/@���@��u@��@�ƨ@��F@�;d@���@���@�=q@���@�7L@���@��j@��j@��@���@�z�@�(�@|�@}��@|��@{t�@z��@z�!@z=q@y�@yG�@x��@x �@w�;@w�@w+@v�y@v��@v�@v��@vv�@vff@v{@u�-@up�@u�@t��@tI�@s�F@so@r-@qX@qG�@qhs@qhs@p��@p  @o�@n�@nȴ@nv�@m��@m`B@m?}@l��@lj@l�@k�m@k��@k@j�!@j��@jM�@i7L@h��@hr�@h �@g�;@g��@g�@g\)@f��@f�+@f$�@e��@eO�@d�D@dI�@d�@c�
@ct�@c"�@b�H@b��@b�\@b~�@b^5@b-@a��@a�7@`��@`bN@_��@_l�@_
=@^��@^{@]�@]`B@]�@\�@\j@[�m@[�@[S�@[o@Y��@Y%@XĜ@X�u@XbN@W�;@W;d@V��@V�R@VE�@U�@U�-@T�@TI�@SdZ@R�@R��@R�!@R�!@Rn�@RJ@Q��@Q&�@Q%@P�`@P��@P�u@Pb@O�w@O\)@N�y@N�R@N��@N5?@M�@M�@M�@M/@L�@Lj@L9X@K��@K�F@K��@K��@K�@KdZ@Ko@J-@I�7@H�`@H1'@G�@G�@Fȴ@Fff@FV@FV@FV@F@E�@E�@D��@D�D@DZ@D1@C��@CC�@C"�@B��@B��@Bn�@B�@A��@A�#@@�`@@�u@@bN@@ �@?
=@>ff@>E�@=��@<�@<Z@<1@;��@;��@;�
@;�F@;��@;�@;S�@;C�@;33@;o@:�H@:�!@:n�@9�7@9%@8��@8Q�@8A�@8b@7��@7��@7K�@7+@7
=@6ȴ@6V@6$�@6{@5�-@5�@5V@4��@4��@4�@4I�@3��@3�
@3��@3S�@2��@1��@17L@1�@1%@0��@0��@0�`@0��@0A�@/�;@/�P@/\)@/\)@/\)@/
=@.��@.@-@-��@-?}@-�@,�j@,Z@+�
@+�F@+o@*^5@)��@)x�@)7L@(�9@( �@'�w@'��@'��@'|�@';d@&�R@&��@&��@&v�@%��@%p�@%`B@%`B@%�@$�@$��@$��@$z�@$Z@$9X@$(�@#��@#�m@#dZ@#33@"��@"��@"�\@"J@!��@ ��@ Q�@ Q�@ Q�@ Q�@ Q�@ Q�@ Q�@ Q�@ bN@ bN@ bN@  �@   @��@��@\)@
=@�R@��@��@�+@v�@V@@�-@p�@�@��@�@�j@z�@(�@�m@�F@��@�@S�@�H@��@��@�\@~�@^5@^5@�@J@��@�@��@��@�7@X@��@�@�@�@Q�@b@�;@�@+@�R@��@�+@{@�@��@�-@�h@p�@`B@?}@V@�j@j@9X@(�@��@��@C�@�H@=q@��@�#@�7@��@�@Q�@�@��@�@�@��@�@`B@V@��@��@�@��@�D@j@Z@Z@I�@9X@9X@1@1@�m@�m@�m@�
@�F@��@�@S�@33@33@33@"�@"�@"�@"�@@
��@
~�@
�@	�^@	��@	G�@	7L@	&�@	&�@	%@�`@Ĝ@�@bN@Q�G�O�A���A���A��A��/A��A��;A��HA��#A��/A��/A��#A��HA��;A��#A��TA��;A��;A��TA��;A��HA��TA��;A��TA��`A��HA��HA��A���A��A���A��A��A���A��A��A��A��A��A��A���A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A���A��A��A���A��A��A��A���A��A��A��A���A��A��A���A���A���A���A���A��A���A���A��A���A���A��A���A���A���A�  A�A�VA�oA�JA�
=A�oA�bA�bA�oA�VA�VA�oA�{A�VA�{A�{A�bA�oA��A�oA�oA��A��A��A��A��A��A��A��A��A�(�A�(�A�$�A�$�A�$�A�"�A�"�A�(�A�&�A�$�A�-A�(�A�(�A�33A�33A�(�A�-A�/A�-A�9XA�ZA�XA�^5A�`BA�ZA�\)A�bNA�ffA�l�A�v�A�r�A�p�A�v�A�v�A�r�A�x�A�x�A�t�A�t�A�z�A�t�A�x�A�z�A�v�A�x�A�|�A�v�A�v�A�z�A�v�A�p�A�v�A�t�A�l�A�l�A�hsA�bNA�^5A�ZA�O�A�Q�A�VA�VA�Q�A�O�A�S�A�VA�O�A�S�A�VA�O�A�M�A�S�A�O�A�Q�A�VA�S�A�Q�A�XA�VA�VA�XA�\)A�XA�VA�ZA�\)A�VA�ZA�bNA�`BA�^5A�dZA�hsA�hsA�n�A�l�A�hsA�hsA�jA�ffA�dZA�ffA�hsA�dZA�dZA�hsA�dZA�ffA�l�A�jA�jA�p�A�r�A�l�A�n�A�t�A�v�A�r�A�v�A�z�A�v�A�x�A�|�A�x�A�v�A�x�A�x�A�r�A�v�A�t�A�r�A�t�A�p�A�n�A�r�A�r�A�n�A�p�A�r�A�l�A�l�A�p�A�n�A�jA�l�A�n�A�hsA�hsA�jA�ffA�dZA�hsA�ffA�dZA�hsA�hsA�dZA�dZA�hsA�dZA�bNA�ffA�dZA�bNA�ffA�dZA�`BA�bNA�dZA�bNA�^5A�bNA�bNA�^5A�`BA�dZA�^5A�^5A�bNA�dZA�`BA�`BA�bNA�dZA�^5A�`BA�dZA�dZA�`BA�dZA�ffA�`BA�`BA�`BA�ZA�S�A�Q�A�VA�O�A�K�A�M�A�O�A�K�A�K�A�O�A�O�A�I�A�I�A�K�A�I�A�G�A�I�A�K�A�E�A�?}A�C�A�A�A�?}A�7LA�;dA�;dA�5?A�33A�7LA�7LA�33A�1'A�5?A�7LA�5?A�1'A�5?A�7LA�33A�/A�33A�5?A�33A�1'A�33A�7LA�33A�/A�33A�33A�1'A�/A�1'A�33A�/A�-A�1'A�33A�-A�/A�33A�5?A�33A�5?A�;dA�9XA�=qA�E�A�I�A�I�A�K�A�O�A�Q�A�O�A�Q�A�S�A�VA�Q�A�S�A�ZA�ZA�VA�XA�\)A�ZA�VA�S�A�Q�A�M�A�E�A�C�A�C�A�A�A�;dA�?}A�A�A�9XA�9XA�;dA�9XA�1'A�-A�(�A�(�A��A�bA�A���A��TAҾwA�x�A�VA�7LA��HA���AѬAэPA�K�A�VA�A��`AжFA�bNA�E�A�ȴA·+A�K�A�?}A�33A��A��A�oA�VA�A�  A�  A���A���A��A��yA��mA��/Aͺ^A͉7ÁA�z�A�Q�A��
A�$�AˍPA�jA�(�Aɣ�A�t�A�9XA���Aȗ�A�VA���Aǡ�AǅAǁA�t�A�K�A�A��Aư!AƃA�$�AŬA�^5AĶFA�bNA�1A��A��HA���Aú^AöFAá�AÏ\A�|�A�v�A�jA�Q�A�;dA�/A�JA��A���A�ĜA¥�A�A�~�A�O�A�+A�{A�bA�1A���A��mA�ƨA��^A��9A���A�|�A�G�A��A�ȴA��RA���A�v�A�G�A�7LA�$�A���A���A���A�v�A�`BA�I�A�"�A�oA�
=A���A��mA��
A�A���A���A��A�ffA�33A�{A���A��HA��
A���A��!A��A�jA�S�A�1A���A���A��-A���A��hA��A�t�A�jA�Q�A�K�A�G�A�7LA�(�A��A�%A���A��A��mA��HA��#A��
A��A���A���A��jA��A���A��hA�p�A�?}A��A��wA���A���A�~�A�S�A�&�A�-A��A�%A���A��A��-A�^5A��A��9A�r�A��#A���A�bNA�5?A�bA��A��yA��`A��mA���A��jA���A��A�VA�33A��yA�\)A�A���A���A��+A�\)A�E�A�"�A��/A��jA���A��DA�l�A�M�A�33A�oA��A���A�A��RA��!A���A���A��PA��\A��DA�n�A�`BA�K�A�  A��#A���A��9A���A�;dA�ƨA�/A�`BA��HA��
A��!A��hA�|�A�dZA�G�A�-A��A�oA���A��HA���A���A���A�ĜA�A�ĜA�A��^A��RA��RA��!A��A���A���A���A���A���A���A���A���A���A���A��\A�t�A�jA�ffA�XA�;dA�+A�(�A��A�%A�A���A��A��mA��/A���A��jA��hA�v�A�ffA�VA�C�A�5?A�&�A��A�A��A��jA���A�oA��A���A���A��PA�t�A�hsA�ZA�E�A�9XA�-A�
=A���A��A��hA�jA�E�A�bA��`A��RA���A�v�A�K�A� �A���A��`A��A���A�ƨA�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                      ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
��B
��B
��B
҉B
҉B
҉B
�TB
��B
�[B
�,B
҉B
��B
�[B
҉B
ҽB
҉B
҉B
҉B
ҽB
ҽB
��B
�&B
��B
�KB
یB
��B
��B
�B
rBB�B�BOBIBB�B%B'�B-B2�B49B5B5?B5tB5tB5?B6zB5B3hB2aB0UB.�B.}B.�B.�B1�B>BDgBJ#BK�BZ�B��B��B�
B��B��B��B�!B|PBn�By>By�B��B��B�wBɆB�TB��B��B��B�`B�cB��B��B�0BбB�5B�2B�B�2B��B�BPB(B�B�B�B.B4BoB(BSB�BPB
rB�B{B �B��B�+B�"BܒB��B�-B�UB�~B��BwfBa�B[WBM�BD�B2aB%B
�PB
��B
��B
�OB
��B
pB
`�B
b�B
:�B
*�B
$�B
�B
�B	��B	��B	�]B	�pB	��B	�!B	��B	��B	��B	��B	�GB	rGB	e�B	`BB	aB	W�B	N<B	DgB	@�B	@�B	;0B	8B	1[B	*0B	'RB	)*B	�B	�B	1B	_B	+B	\B	�B	
�B	�B	+B	GB	�B�JB��B��B��B��B�B�DB�xB�PB�PB	 �B	AB	�B	�B	B	�B	�B	1B	
rB	
�B	~B	�B	PB	xB	�B	�B	�B	B	4B	B	�B	eB	OB	#:B	&B	(XB	)*B	'�B	'B	)�B	$@B	%FB	"�B	�B	!B	~B	�B	�B	kB	�B	�B	+B	OB	xB	CB	B	�B	�B	�B	#nB	 �B	$tB	&B	$@B	"�B	�B	!bB	SB	uB	bB	�B	�B	�B	B	�B	�B	oB	eB	uB	B	�B	IB	"�B	,�B	1�B	5tB	1�B	1�B	3hB	:^B	C�B	>BB	A�B	C�B	FB	F�B	G�B	H�B	H�B	IB	L�B	NpB	P}B	S�B	ZB	Z�B	[WB	^B	_;B	`�B	e�B	i�B	jB	m�B	q�B	q�B	tB	t�B	t�B	t�B	{B	~]B	~]B	�uB	�DB	�xB	��B	�"B	�MB	�=B	�qB	��B	��B	�xB	��B	��B	�B	�=B	�=B	��B	�aB	�B	��B	��B	�BB	�OB	�[B	�gB	��B	ŢB	��B	�EB	�zB	��B	��B	�KB	��B	�B	��B	��B	�sB	�QB	��B	��B	�B	�fB	�mB	��B	�B	�B	�B	��B	��B	��B	��B	�8B	��B	�(B
B
 �B
B
�B
_B
�B
�B
	B

	B
xB
�B
�B
�B
�B
�B
�B
{B
SB
�B
�B
�B
�B
YB
�B
�B
$B
_B
=B
CB
�B
�B
�B
�B
�B
B
=B
#:B
%FB
&B
&�B
&�B
'RB
(�B
(�B
)�B
*�B
*eB
*eB
)�B
(�B
'�B
&�B
'�B
($B
(�B
)�B
*�B
,=B
-B
/�B
0�B
1[B
2�B
49B
3�B
3�B
3�B
3hB
2�B
2aB
1[B
2�B
49B
5tB
5�B
6B
7B
7�B
8B
8�B
9$B
8�B
9XB
9�B
9�B
:*B
:*B
:�B
;0B
;dB
;dB
;dB
<6B
<B
<�B
<jB
<jB
>�B
>�B
?B
?�B
@�B
A�B
A B
@�B
@�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
C�B
C�B
D�B
FtB
F?B
F?B
F�B
F�B
GB
G�B
G�B
HB
H�B
H�B
H�B
IB
IB
K)B
MB
LdB
NB
OvB
O�B
O�B
O�B
O�B
PHB
P}B
Q�B
Q�B
Q�B
Q�B
Q�B
R B
R B
RTB
R�B
R�B
T�B
UgB
U�B
U�B
U�B
U2B
T�B
V�B
XyB
X�B
XEB
X�B
X�B
YB
ZQB
Y�B
YB
ZQB
ZB
ZB
ZQB
Z�B
\]B
Z�B
[WB
\�B
]/B
^5B
]�B
]/B
]/B
]�B
]�B
]dB
]/B
]�B
]�B
^5B
_pB
`�B
c�B
c�B
dZB
d�B
d�B
e,B
e�B
e�B
gB
g�B
hsB
h>B
hsB
h>B
hsB
h
B
iB
iyB
iB
jB
kB
j�B
j�B
lWB
l"B
k�B
l�B
l�B
m]B
ncB
n/B
m�B
m�B
m�B
n/B
o B
n/B
ncB
m�B
m]B
l�B
l�B
l�B
m]B
m�B
m�B
n/B
m�B
n�B
o B
oiB
pB
p�B
pB
oiB
o�B
pB
p�B
rB
r�B
r�B
r|B
s�B
tTB
t�B
s�B
s�B
s�B
v�B
w2B
w�B
w�B
wfB
v�B
v�B
v�B
v�B
v�B
v�B
xB
xlB
x8B
xlB
x8B
x8B
xB
x8B
y	B
y	B
yrB
yrB
y�B
y�B
y�B
y�B
y�B
zxB
zxB
zB
z�B
{�B
{B
{�B
|B
|�B
|�B
|�B
}"B
}VB
}VB
}VB
}VB
}VB
~(B
~�B
�B
� B
�iB
��B
�B
�oB
��B
�oB
��B
�B
�AB
��B
�B
��B
�B
�MB
�MB
�MB
��B
��B
�SB
��B
��B
�%B
�YB
�YB
��B
��B
�_B
��B
�1B
��B
�1B
��B
�rB
��B
�7B
�lB
�7B
�lB
�lB
��B
�	B
�=B
��B
��B
�B
�DB
�B
�B
�JB
��B
�JB
��B
�PB
��B
�"B
�"B
�"B
�"B
�"B
��B
�"B
��B
��B
��B
� B
��B
� B
��B
�:B
�:B
�:B
�oB
�B
�B
�{B
�FB
�B
�B
�{B
��B
�$B
�$B
��B
��B
��B
�+B
�+B
��B
��B
�eB
�1B
��B
�qB
�	B
��B
��B
�B
�CB
�B
��B
��B
��B
��B
��B
�B
�CB
�CB
�xB
��B
��B
��B
�B
�OB
�OB
��B
��B
�!B
�!B
�VB
��B
��B
��B
��B
�\B
��B
��B
��B
�bB
��B
��B
��B
��B
��B
��B
�4B
�4B
�hB
��B
�:B
��B
�tB
��B
��B
��B
��B
��B
��B
�zB
�zB
��B
��B
��B
��B
�B
��B
��B
��B
��B
�RB
�RB
��B
��B
��B
��B
�*B
��B
�0B
�eB
��B
�kB
��B
�B
�=B
�B
�=B
�qB
��B
�B
�B
�B
��B
�B
�}B
�}B
��B
��B
�B
�OB
�OB
�OB
�OB
�OB
��B
��B
��B
��B
�[B
�'B
�'B
��B
��B
��B
�aB
�aB
�aB
�aB
�aB
�-B
�aB
�aB
�aB
�-B
�aB
�aB
�-B
�-B
�-B
�-B
��B
�hB
��B
��B
��B
��B
��B
�nB
��B
��B
�B
��B
�B
��B
��B
�?B
��B
��B
��B
�B
�B
�FB
�zB
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�LB
�LB
��B
�RB
��B
��B
��B
��B
��B
�*B
��B
�dB
�dB
�dB
�dB
�B
�B
�B
�6B
�6B
�6B
�6B
�jB
�jB
��B
��B
��B
��B
��B
�B
�B
�B
��B
�jB
�6B
�6B
��B
�dB
��B
��B
��B
��B
�B
��B
�B
�B
�B
�B
�B
�B
�B
�6B
�jB
�jB
�jB
�jB
��B
��B
��B
��B
��B
��B
��B
��B
�B
�<B
�<B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�wB
��B
�B
�HB
��B
��B
��B
��B
��B
��B
��B
�B
�B
��B
��B
�,B
�,B
�aB
҉B
�2B
ӏB
� B
ԕB
�TB
�&B
�&B
� B
ѷB
�[B
�NB
�[B
ҽB
��B
�[B
ҽB
ѷB
�[B
҉B
ѷB
�[B
�[B
ΥB
ԕB
�sB
��B
�&B
��B
� B
��B
�aB
�&B
уB
��B
�,B
ҽB
��B
��B
ѷB
҉B
��B
� B
�NB
��B
уB
��B
ӏB
�&B
��B
ҽB
�[B
ѷB
ҽB
�[B
��B
ҽB
��B
�TB
уB
ӏB
�&B
уB
ӏB
�&B
уB
ҽB
ӏB
уB
��B
��B
�aB
��B
ҽB
�[B
ҽB
� B
�,B
�[B
бB
�9B
�gB
�,B
�TB
�9B
҉B
҉B
��B
� B
��B
�aB
�&B
� B
��B
ѷB
�EB
�EB
�B
�B
خB
��B
�B
��B
�B
ٴB
ںB
�B
�B
�WB
�KB
��B
چB
�KB
��B
�QB
��B
�KB
یB
��B
�HB
�B
�TB
�B
�B
��B
�B
��B
�B
�KB
�WB
��B
�)B
�"B
��B
��B
�B
��B
�B
��B
�B
�)B
�cB
�)B
�B
�/B
�cB�BxB
	B"B�B	lB\B�B@B�B�B�B�B$B�BB�B+B�B�B�B_BB7B1B�B	B�B=B�B=B�B �B�B�B!-B�BBBIBCBB�B�B�BxBOBIBCBB�BBBB�B�B!B~B~B�B!B�B�B�B�BB \B!B�B!�B"hB �B!�B%zB%B%FB'�B&B%zB&�B'RB&�B%zB&�B'RB%�B($B'�B'RB*0B*�B*0B*�B-CB-CB+kB-CB/�B/B/B1'B1'B1'B3�B49B2-B33B5?B33B3�B4�B33B4�B5?B3hB3�B5?B4nB3�B5?B5tB3�B49B5�B5?B49B6FB5tB4B5�B6B4B4�B5�B4�B49B6B6B4�B5�B6FB4�B5B6�B4�B5B6zB5�B4�B5?B6zB5?B4nB6�B5�B49B6FB6zB4�B4nB6B6B4�B4�B6�B6FB4�B6FB7�B6�B5tB7LB7B6zB5B5?B5tB3�B49B4nB2�B2�B49B49B2�B2�B4B3�B2�B2�B33B2-B1�B2�B49B0�B0!B1�B1�B0!B/B1�B0UB.�B.IB/OB0!B.�B-�B.}B/�B.}B-wB/OB/�B-�B-CB-�B/�B/B-�B/B/�B-�B-�B/B/�B.�B-wB/�B/�B.IB.IB/�B/OB.IB.}B0�B33B1[B1'B5�B6zB9$B<B?HB=qB>�B@�BA�BA BB'BC�BC�BCaBE9BG�BGBFtBG�BIBJ�BI�BK)BL�BLdBK)BK�BMBK�BJ�BL�BMBL�BL�BPHBS�BYB[�Bd&Bj�Bp�BqvBwfB�oB�{B�eB�VB��B��B��B��B�KB�dB��BбB�9B��B�)B��B� B�&B�B�B�TB�HB�B��B�ZB��B�B��B��B�ZB�|B�;B�vB��B�TB��B�WB� B�2B��B��B��B�XB�XB��B��B�3B�*B��B��B��B�JB��B��B�B��Bz�ByrBr|B|�Bt�Bl�ByrBrBm�Bh�BjBn/BoiBl�Bo�Bs�BxlBx�B{B|�B~�BzDBy�BzDBu%Bt�Bv+Bs�BxBzDB.B�AB��B�B��B��B��B�B��B��B��B�\B�!B�eB�B��B�-B��B�B�B�wB��B�}B�BB�B�-B�gB�-B��B��BȀB�EB�)BуB˒B�BбB� B�2B�TB�TB�NBҽB�9B�9B��B�B��B�
B�B�B�B�;B�/B��B��B��B�iB�/B�vB��B��B��B��B�`B�B�B�ZB�%B�B�B��B�>B�lB�2B��B��B{B	7BuB��B�JB��B��B�/B�B�vB�B�8B��B�QB��B�B��BچB��B��B�
B��B�^B�6B˒B�#B�KB��B�B�B�#B҉B�B�pB�]B�B��B�WB�B�;B�5B�;B�mB��B��B��B��B�B�B��B�B�B�fB��B�>B�B�B�2B�B�&B�2B��B��B�AB�yB�yB�BMB�B�.B1B�B:B+BB~BB�B�B�B�BPB�B:B�B�B"B�B\B�B�B�B(B�B(B�B�B�B\B�B\B(B�B�B"B�B�BhB�B"B.B:BbB�B:B@B�BB.B BoBB�BBB�B�B�B�B�B�BVBPB�B>BhB�BVB�B�BVB�B~BB�BBBhB	�B�B(B�BB�BPBB	�B�B�B
	BBB�B�BG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                      G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                      G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No good data in profile due to conductivity sensor failure after strong salinity drift;                                                                                                                                                                         SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No good data in profile due to conductivity sensor failure after strong salinity drift;                                                                                                                                                                         202104292026552021042920265520210429202655202104292026552021042920265520210429202655SI  SI  ARFMARFM                                                                                                                                                2021012820273620210128202736IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021020723010420210207230104QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021020723010420210207230104QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2021042910194120210429101941IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV2.1V2.1                                                                                                                                2021042920270320210429202703IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOW  OW  V1.0V1.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2021042920270320210429202703IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2021042920270320210429202703IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                